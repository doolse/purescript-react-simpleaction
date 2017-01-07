module React.SimpleAction.Dispatch where

import Prelude
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Function.Eff (EffFn1, mkEffFn1)
import Data.Maybe (Maybe, maybe)
import React (ReactElement, ReactThis)

class Dispatchable eval a context | eval -> context where
  dispatchEff :: forall eff. eval -> a -> context -> Eff eff Unit

instance funcMaybeDispatch :: Dispatchable m Unit context => Dispatchable (a -> m) (Maybe a) context where
  dispatchEff f ma this = maybe (pure unit) (\a -> dispatchEff (f a) unit this) ma

instance funcDispatch :: Dispatchable m Unit context => Dispatchable (a -> m) a context where
  dispatchEff f a = dispatchEff (f a) unit

instance readerTTest :: Dispatchable (m eff Unit) Unit context => Dispatchable (ReaderT context (m eff) Unit) Unit context where
  dispatchEff ma _ this = dispatchEff (runReaderT ma this) unit this

instance flippedDispatcher :: Dispatchable a Unit context => Dispatchable Unit a context where
  dispatchEff = flip dispatchEff

instance effDispacher :: Dispatchable (Eff eff Unit) Unit context where
  dispatchEff e _ _ = unsafeCoerceEff e

instance affDispacher :: Dispatchable (Aff eff Unit) Unit context where
  dispatchEff a _ _ = unsafeCoerceEff $ void $ launchAff a

class EvalRenderer dispatcher result eval props state | dispatcher -> result props state where
  evalRender :: ReactThis props state -> eval -> dispatcher -> result

instance elemRenderable :: EvalRenderer ReactElement ReactElement eval props state where
  evalRender _ _ = id

newtype DispatchEff a = DispatchEff (forall ev eff. (ev -> a) -> ev -> Eff eff Unit)

newtype DispatchEffFn a = DispatchEffFn (forall ev eff. (ev -> a) -> EffFn1 eff ev Unit)

instance dispatchRenderable :: (Dispatchable eval a (ReactThis props state), EvalRenderer result ReactElement eval props state) => EvalRenderer (DispatchEff a -> result) ReactElement eval props state where
  evalRender t eval f = evalRender t eval $ f $ DispatchEff (\handle ev -> dispatchEff eval (handle ev) t)

instance dispatchEffFn :: (Dispatchable eval a (ReactThis props state), EvalRenderer result ReactElement eval props state) => EvalRenderer (DispatchEffFn a -> result) ReactElement eval props state where
  evalRender t eval f = evalRender t eval $ f $ DispatchEffFn (\handle -> mkEffFn1 \ev -> dispatchEff eval (handle ev) t)
