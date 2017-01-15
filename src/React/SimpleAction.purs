module React.SimpleAction where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Reader (ReaderT(ReaderT))
import Data.Function.Uncurried (Fn4, runFn4)
import React (Disallowed, GetInitialState, ReactElement, ReactProps, ReactRefs, ReactSpec, ReactState, ReactThis, Read, ReadOnly, ReadWrite, Refs, Render, readState, spec', transformState)
import React (getProps, getRefs) as R
import React.SimpleAction.Dispatch (class Dispatchable, class FromContext, State(..), dispatch, fromContext)


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


didMount :: forall eval props state action eff. Dispatchable eval (ReactThis props state) action (CallbackEff eff Unit) => action -> eval -> ReactSpec props state eff -> ReactSpec props state eff
didMount a e = _ { componentDidMount = flip (dispatch e) a }


class ReactSpecCreator eval initialstate render props state eff | eval initialstate render -> props state eff where
  createSpec :: eval -> initialstate -> (state -> render) -> ReactSpec props state eff

class ReactInitialState eval initialstate props state eff | initialstate -> state where
  createInitialState :: eval -> initialstate -> GetInitialState props state eff

class ReactRender eval renderer props state eff where
  createRenderer :: eval -> renderer -> Render props state eff

instance dftRSC :: (
    ReactInitialState eval initialstate props state eff
  , ReactRender eval renderer props state eff)
  => ReactSpecCreator eval initialstate renderer props state eff where
  createSpec eval initial renderer = spec' (createInitialState eval initial) \this -> do
    s <- readState this
    (createRenderer eval $ renderer s) this

instance iss :: -- TypeEquals state2 state =>
  ReactInitialState eval (State state) props state eff where
  createInitialState eval (State s) = const $ pure s

instance dftIS :: (
  FromContext eval (ReactThis props state) r Eff (props :: ReactProps, refs::ReactRefs (), state::ReactState ()|eff)
  , ReactInitialState eval next props state eff)
  => ReactInitialState eval (r -> next) props state eff where
  createInitialState eval initial = \this -> do
    r <- fromContext eval this
    createInitialState eval (initial r) this

instance renderElem :: ReactRender eval ReactElement props state eff where
  createRenderer _ = const <<< pure

instance dftRR :: (
    ReactRender eval next props state eff
  , FromContext eval (ReactThis props state) r Eff (props :: ReactProps, refs::ReactRefs (), state::ReactState (read::Read)|eff)
  )
  => ReactRender eval (r -> next) props state eff where
  createRenderer eval f = \this -> do
    r <- fromContext eval this
    createRenderer eval (f r) this
