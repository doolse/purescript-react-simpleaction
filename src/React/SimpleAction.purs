module React.SimpleAction where

import Prelude
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import Data.Function.Eff (EffFn1, mkEffFn1)
import Data.Function.Uncurried (Fn4, runFn4)
import React (ReactElement, ReactProps, ReactRefs, ReactState, ReactThis, ReadOnly, ReadWrite, Refs, Render, readState, transformState)
import React (getProps, getRefs) as R

type ReactReaderT props state m a = ReaderT (ReactThis props state) m a

reactReaderT :: forall props state m a. (ReactThis props state -> m a) -> ReactReaderT props state m a
reactReaderT = ReaderT

stateRenderer :: forall props state eff. (ReactThis props state -> state -> ReactElement) -> Render props state eff
stateRenderer f this = do
  p <- readState this
  pure $ f this p

propsRenderer :: forall props state eff. (ReactThis props state -> props -> ReactElement) -> Render props state eff
propsRenderer f this = do
  p <- R.getProps this
  pure $ f this p

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

class Dispatchable m props state eff | m -> props, m -> state, m -> eff where
  dispatchEff :: m -> ReactThis props state -> Eff eff Unit

instance readerTTest :: Dispatchable (m eff Unit) props state eff => Dispatchable (ReaderT (ReactThis props state) (m eff) Unit) props state eff where
  dispatchEff ma this = dispatchEff (runReaderT ma this) this

instance effDispacher :: Dispatchable (Eff eff Unit) props state eff where
  dispatchEff e _ = e

instance affDispacher :: Dispatchable (Aff eff Unit) props state eff where
  dispatchEff a _ = unsafeCoerceEff $ void $ launchAff a

handle :: forall props state eff m ev. (Dispatchable m props state eff) => ReactThis props state -> (ev -> m) -> EffFn1 eff ev Unit
handle this f = mkEffFn1 \ev -> dispatchEff (f ev) this

handleEff :: forall props state eff ev. ReactThis props state -> (ev -> ReactReaderT props state (Eff eff) Unit) -> EffFn1 eff ev Unit
handleEff = handle

newtype Dispatcher eff a = Dispatcher (forall ev. (ev -> a) -> EffFn1 eff ev Unit)

withDispatcher :: forall eff p s a b m. Dispatchable m p s eff => (a -> m) -> (Dispatcher eff a -> b) -> ReactThis p s -> b
withDispatcher evalf r this = r (Dispatcher \f -> mkEffFn1 \ev -> dispatchEff (evalf $ f ev) this)
