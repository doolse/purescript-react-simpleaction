module React.SimpleAction.Dispatch where

import Prelude
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Function.Eff (EffFn1, mkEffFn1)
import Data.Maybe (Maybe, maybe)
import React (ReactElement, ReactThis)

class Dispatchable eval a props state | eval -> props state where
  dispatchEff :: forall eff. eval -> a -> ReactThis props state -> Eff eff Unit

instance funcMaybeDispatch :: Dispatchable m Unit props state => Dispatchable (a -> m) (Maybe a) props state where
  dispatchEff f ma this = maybe (pure unit) (\a -> dispatchEff (f a) unit this) ma

instance funcDispatch :: Dispatchable m Unit props state => Dispatchable (a -> m) a props state where
  dispatchEff f a = dispatchEff (f a) unit

instance readerTTest :: Dispatchable (m eff Unit) Unit props state => Dispatchable (ReaderT (ReactThis props state) (m eff) Unit) Unit props state where
  dispatchEff ma _ this = dispatchEff (runReaderT ma this) unit this

instance effDispacherFlipped :: Dispatchable (Eff eff Unit) Unit props state where
  dispatchEff = flip dispatchEff

instance effDispacher :: Dispatchable Unit (Eff eff Unit) props state where
  dispatchEff _ e _ = unsafeCoerceEff e

instance affDispacher :: Dispatchable (Aff eff Unit) Unit props state where
  dispatchEff a _ _ = unsafeCoerceEff $ void $ launchAff a

class EvalRenderer dispatcher result eval props state | dispatcher -> result props state where
  evalRender :: ReactThis props state -> eval -> dispatcher -> result

instance elemRenderable :: EvalRenderer ReactElement ReactElement eval props state where
  evalRender _ _ = id

newtype DispatchEff a = DispatchEff (forall ev eff. (ev -> a) -> ev -> Eff eff Unit)

newtype DispatchEffFn a = DispatchEffFn (forall ev eff. (ev -> a) -> EffFn1 eff ev Unit)

instance dispatchRenderable :: (Dispatchable eval a props state, EvalRenderer result ReactElement eval props state) => EvalRenderer (DispatchEff a -> result) ReactElement eval props state where
  evalRender t eval f = evalRender t eval $ f $ DispatchEff (\handle ev -> dispatchEff eval (handle ev) t)

instance dispatchEffFn :: (Dispatchable eval a props state, EvalRenderer result ReactElement eval props state) => EvalRenderer (DispatchEffFn a -> result) ReactElement eval props state where
  evalRender t eval f = evalRender t eval $ f $ DispatchEffFn (\handle -> mkEffFn1 \ev -> dispatchEff eval (handle ev) t)
