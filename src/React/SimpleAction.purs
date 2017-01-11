module React.SimpleAction where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Reader (ReaderT(ReaderT))
import Data.Function.Uncurried (Fn4, runFn4)
import React (ReactClass, ReactElement, ReactProps, ReactRefs, ReactState, ReactThis, ReadOnly, ReadWrite, Refs, Render, ReactSpec, createClass, readState, spec, transformState)
import React (getProps, getRefs) as R
import React.SimpleAction.Dispatch (class Dispatchable, class EvalRenderer, dispatchEff, evalRender)

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

createRenderer :: forall renderable props state eval eff. (EvalRenderer renderable ReactElement eval (ReactThis props state)) => (state -> renderable) -> eval -> Render props state eff
createRenderer f e t = do
  s <- readState t
  pure $ evalRender t e (f s)

createRendererPS :: forall renderable props state eval eff. (EvalRenderer renderable ReactElement eval (ReactThis props state))  => (props -> state -> renderable) -> eval -> Render props state eff
createRendererPS f e t = do
  p <- R.getProps t
  createRenderer (f p) e t

createComponent :: forall props state renderable eval. EvalRenderer renderable ReactElement eval (ReactThis props state) => state -> (state -> renderable) -> eval -> ReactClass props
createComponent = createComponent' id

createComponent' :: forall props state renderable eval eff. EvalRenderer renderable ReactElement eval (ReactThis props state) =>
  (ReactSpec props state eff -> ReactSpec props state eff)
  -> state
  -> (state -> renderable)
  -> eval
  -> ReactClass props
createComponent' f s r e = createClass $ f $ spec s $ createRenderer r e

createComponentPS :: forall props state renderable eval. EvalRenderer renderable ReactElement eval (ReactThis props state) => state -> (props -> state -> renderable) -> eval -> ReactClass props
createComponentPS = createComponentPS' id

createComponentPS' :: forall props state renderable eval eff. EvalRenderer renderable ReactElement eval (ReactThis props state) =>
  (ReactSpec props state eff -> ReactSpec props state eff)
  -> state
  -> (props -> state -> renderable)
  -> eval
  -> ReactClass props
createComponentPS' f s r e = createClass $ f $ spec s $ createRendererPS r e

didMount :: forall eval props state eff. Dispatchable eval Unit (ReactThis props state) => eval -> ReactSpec props state eff -> ReactSpec props state eff
didMount e = _ { componentDidMount = flip (dispatchEff e) unit }
