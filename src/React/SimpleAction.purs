module React.SimpleAction where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Reader (ReaderT(ReaderT))
import Data.Function.Uncurried (Fn4, runFn4)
import React (Disallowed, GetInitialState, ReactClass, ReactElement, ReactProps, ReactRefs, ReactSpec, ReactState, ReactThis, ReadOnly, ReadWrite, Refs, Render, createClass, readState, spec', transformState)
import React (getProps, getRefs) as R
import React.SimpleAction.Dispatch (class Dispatchable, dispatch)
import Unsafe.Coerce (unsafeCoerce)

type ReactReaderT props state m a = ReaderT (ReactThis props state) m a

reactReaderT :: forall props state m a. (ReactThis props state -> m a) -> ReactReaderT props state m a
reactReaderT = ReaderT

getProps :: forall m props state eff. MonadEff ( props :: ReactProps | eff ) m => ReactReaderT props state m props
getProps =  reactReaderT (liftEff <<< R.getProps)

modifyState :: forall m props state eff. MonadEff ( state :: ReactState ReadWrite | eff ) m => (state -> state) -> ReactReaderT props state m Unit
modifyState f = reactReaderT (\this -> liftEff $ transformState this f)

getState :: forall m props state eff. MonadEff ( state :: ReactState ReadWrite | eff ) m => ReactReaderT props state m state
getState = reactReaderT (liftEff <<< readState)

foreign import mapRef :: forall a ref. Fn4 (ref -> a) a Refs String a

getRefs :: forall eff props state m. (MonadEff (refs :: ReactRefs ReadOnly|eff) m) => ReactReaderT props state m Refs
getRefs = reactReaderT (liftEff <<< R.getRefs)

unsafeWithRef :: forall props state ref eff m. (MonadEff (refs::ReactRefs ReadOnly|eff) m) =>
  (ref -> Eff (refs::ReactRefs ReadOnly|eff) Unit)
  -> String
  -> ReactReaderT props state m Unit
unsafeWithRef f s = do
  refs <- getRefs
  liftEff $ runFn4 mapRef f (pure unit) refs s

emptyHandler :: forall a m. (Applicative m) => a -> m Unit
emptyHandler = const $ pure unit

type RenderEff eff = Eff (props::ReactProps, refs::ReactRefs Disallowed, state :: ReactState ReadOnly | eff) ReactElement
type CallbackEff eff a = Eff ( props :: ReactProps, state :: ReactState ReadWrite, refs :: ReactRefs ReadOnly | eff) a
type InitialStateEff eff state= Eff ( props :: ReactProps, state :: ReactState Disallowed, refs :: ReactRefs Disallowed | eff) state

createRenderer :: forall renderable props state eval eff. Dispatchable eval (ReactThis props state) renderable (RenderEff eff) =>
  (state -> renderable)
  -> eval
  -> Render props state eff
createRenderer f e t = do
  s <- readState t
  dispatch e t (f s)

createRendererPS :: forall renderable props state eval eff. Dispatchable eval (ReactThis props state) renderable (RenderEff eff) => (props -> state -> renderable) -> eval -> Render props state eff
createRendererPS f e t = do
  p <- R.getProps t
  createRenderer (f p) e t


createComponent :: forall props state renderable eval eff. ReactRender (state -> renderable) eval props state eff
  => state -> (state -> renderable) -> eval -> ReactClass props
createComponent = createComponent' (const $ id :: ReactSpec props state eff -> ReactSpec props state eff)

createComponent' :: forall props state initialstate renderable eval eff. (
    ReactRender (state -> renderable) eval props state eff
  , ReactGetInitialState initialstate eval props state eff
  )
  => (eval -> ReactSpec props state eff -> ReactSpec props state eff)
  -> initialstate
  -> (state -> renderable)
  -> eval
  -> ReactClass props
createComponent' f s r e = createClass $ f e $ spec' (createInitialState e s) $ createRenderer2 e r

createComponentPS :: forall props state renderable eval eff. Dispatchable eval (ReactThis props state) renderable (RenderEff eff)
  => state -> (props -> state -> renderable) -> eval -> ReactClass props
createComponentPS = createComponentPS' (const $ id :: ReactSpec props state eff -> ReactSpec props state eff)

createComponentPS' :: forall props state initialstate renderable eval eff. (
    ReactGetInitialState initialstate eval props state eff
  , Dispatchable eval (ReactThis props state) renderable (Eff (props::ReactProps, refs::ReactRefs Disallowed, state :: ReactState ReadOnly | eff) ReactElement)) =>
  (eval -> ReactSpec props state eff -> ReactSpec props state eff)
  -> initialstate
  -> (props -> state -> renderable)
  -> eval
  -> ReactClass props
createComponentPS' f s r e = createClass $ f e $ spec' (createInitialState e s) $ createRendererPS r e

didMount :: forall eval props state action eff. Dispatchable eval (ReactThis props state) action (CallbackEff eff Unit) => action -> eval -> ReactSpec props state eff -> ReactSpec props state eff
didMount a e = _ { componentDidMount = flip (dispatch e) a }


class ReactGetInitialState initialstate eval props state eff | initialstate -> props state eff where
  createInitialState :: eval -> initialstate -> GetInitialState props state eff

instance dispatchState :: Dispatchable eval (ReactThis props state) initialstate (Eff (props :: ReactProps, state :: ReactState (), refs :: ReactRefs () | eff) state)
  => ReactGetInitialState initialstate eval props state eff where
  createInitialState e s = \this -> dispatch e this s

class ReactRender renderer eval props state eff | eval renderer -> props state eff where
  createRenderer2 :: eval -> renderer -> Render props state eff

instance stateFunc :: ReactRender (state -> something) eval props state eff where
  createRenderer2 = unsafeCoerce
