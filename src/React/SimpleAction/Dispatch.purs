module React.SimpleAction.Dispatch where

import Prelude
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Function.Eff (EffFn1, mkEffFn1)
import Data.Maybe (Maybe, maybe)
import React (ReactElement)

class Dispatchable eval a context | eval -> context where
  dispatchEff :: forall eff. eval -> context -> a -> Eff eff Unit

instance funcMaybeDispatch :: Dispatchable m Unit context => Dispatchable (a -> m) (Maybe a) context where
  dispatchEff f this ma = maybe (pure unit) (\a -> dispatchEff (f a) this unit) ma

instance funcDispatch :: Dispatchable m Unit context => Dispatchable (a -> m) a context where
  dispatchEff f c a = dispatchEff (f a) c unit

instance readerTTest :: Dispatchable (m eff Unit) Unit context => Dispatchable (ReaderT context (m eff) Unit) Unit context where
  dispatchEff ma this _ = dispatchEff (runReaderT ma this) this unit

instance flippedDispatcher :: Dispatchable a Unit context => Dispatchable Unit a context where
  dispatchEff e c a = dispatchEff a c e

instance effDispacher :: Dispatchable (Eff eff Unit) Unit context where
  dispatchEff e _ _ = unsafeCoerceEff e

instance affDispacher :: Dispatchable (Aff eff Unit) Unit context where
  dispatchEff a _ _ = unsafeCoerceEff $ void $ launchAff a

newtype EffEval eff r a = EffEval (a -> ReaderT r (Eff eff) Unit)

instance effEvalDispatcher :: Dispatchable (a -> ReaderT context (Eff eff) Unit) a' context => Dispatchable (EffEval eff context a) a' context where
  dispatchEff (EffEval e) = dispatchEff e

class EvalRenderer dispatcher result eval context | dispatcher -> result context where
  evalRender :: context -> eval -> dispatcher -> result

instance elemRenderable :: EvalRenderer ReactElement ReactElement eval context where
  evalRender _ _ = id

newtype DispatchEff a = DispatchEff (forall ev eff. (ev -> a) -> ev -> Eff eff Unit)

newtype DispatchEffFn a = DispatchEffFn (forall ev eff. (ev -> a) -> EffFn1 eff ev Unit)

instance dispatchRenderable :: (Dispatchable eval a context, EvalRenderer next result eval context) => EvalRenderer (DispatchEff a -> next) result eval context where
  evalRender t eval f = evalRender t eval $ f $ DispatchEff (\handle ev -> dispatchEff eval t (handle ev))

instance dispatchEffFn :: (Dispatchable eval a context, EvalRenderer next result eval context) => EvalRenderer (DispatchEffFn a -> next) result eval context where
  evalRender t eval f = evalRender t eval $ f $ DispatchEffFn (\handle -> mkEffFn1 \ev -> dispatchEff eval t (handle ev))
