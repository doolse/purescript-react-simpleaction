module React.SimpleAction.Dispatch where

import Prelude
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Function.Eff (EffFn1, EffFn2, EffFn3, mkEffFn1, mkEffFn2, mkEffFn3)
import Data.Maybe (Maybe, maybe)
import React (ReactProps, ReactState, ReactThis, Read, getProps, readState)
import Type.Equality (class TypeEquals, to)

class Dispatchable eval context action result | eval -> context where
  dispatch :: eval -> context -> action -> result

instance funcMaybeDispatch :: (Applicative m, Dispatchable Unit context dispatchable (m Unit)) => Dispatchable (action -> dispatchable) context (Maybe action) (m Unit) where
  dispatch f c ma = maybe (pure unit) (\a -> dispatch (f a) c unit) ma

instance funcDispatch :: Dispatchable Unit context m result => Dispatchable (action -> m) context action result where
  dispatch f c a = dispatch unit c (f a)

instance readerTTest :: Dispatchable eval context (m a) result => Dispatchable eval context (ReaderT context m a) result where
  dispatch e c a = dispatch e c $ runReaderT a c

instance applicativeDispatcher :: Applicative m => Dispatchable eval context a (m a) where
  dispatch _ _ = pure

instance effDispacher :: Dispatchable eval context (Eff eff a) (Eff eff2 a) where
  dispatch _ _ = unsafeCoerceEff

instance affDispacher :: Dispatchable eval context (Aff eff a) (Eff eff2 Unit) where
  dispatch _ _ = void <<< unsafeCoerceEff <<< launchAff

effEval ::  forall a context eff. (a -> ReaderT context (Eff eff) Unit) -> a -> ReaderT context (Eff eff) Unit
effEval = id

newtype DispatchEff a = DispatchEff (forall ev eff. (ev -> a) -> ev -> Eff eff Unit)

newtype DispatchAff a = DispatchAff (forall ev eff. (ev -> a) -> ev -> Aff eff Unit)

newtype DispatchEffFn a = DispatchEffFn (forall ev eff. (ev -> a) -> EffFn1 eff ev Unit)

newtype DispatchEffFn2 a = DispatchEffFn2 (forall ev ev2 eff. (ev -> ev2 -> a) -> EffFn2 eff ev ev2 Unit)

newtype DispatchEffFn3 a = DispatchEffFn3 (forall ev ev2 ev3 eff. (ev -> ev2 -> ev3 -> a) -> EffFn3 eff ev ev2 ev3 Unit)

instance dispatchEffD :: (Applicative (m eff), Dispatchable eval context action (Eff eff Unit))
  => FromContext eval context (DispatchEff action) m eff where
  fromContext eval c = pure $ DispatchEff (\handle ev -> unsafeCoerceEff $ (dispatch eval c (handle ev)) :: Eff eff Unit)

instance dispatchEffFnD :: (Applicative (m eff), Dispatchable eval context action (Eff eff Unit))
  => FromContext eval context (DispatchEffFn action) m eff where
  fromContext eval c = pure $ DispatchEffFn (\handle -> mkEffFn1 \ev -> unsafeCoerceEff $ (dispatch eval c (handle ev)) :: Eff eff Unit)

instance dispatchEffFn2D :: (Applicative (m eff), Dispatchable eval context action (Eff eff Unit))
  => FromContext eval context (DispatchEffFn2 action) m eff where
  fromContext eval c = pure $ DispatchEffFn2 (\handle -> mkEffFn2 \ev ev2 -> unsafeCoerceEff $ (dispatch eval c (handle ev ev2)) :: Eff eff Unit)

instance dispatchEffFn3D :: (Applicative (m eff), Dispatchable eval context action (Eff eff Unit))
  => FromContext eval context (DispatchEffFn3 action) m eff where
  fromContext eval c = pure $ DispatchEffFn3 (\handle -> mkEffFn3 \ev ev2 ev3 -> unsafeCoerceEff $ (dispatch eval c (handle ev ev2 ev3)) :: Eff eff Unit)

dispatchEff :: forall ev action eff. DispatchEff action -> (ev -> action) -> ev -> Eff eff Unit
dispatchEff (DispatchEff d) = d

dispatchEffFn :: forall ev action eff. DispatchEffFn action -> (ev -> action) -> EffFn1 eff ev Unit
dispatchEffFn (DispatchEffFn d) = d

dispatchEff_ :: forall ev action eff. DispatchEff action -> action -> ev -> Eff eff Unit
dispatchEff_ (DispatchEff d) a = d $ const a

dispatchEffFn_ :: forall ev action eff. DispatchEffFn action -> action -> EffFn1 eff ev Unit
dispatchEffFn_ (DispatchEffFn d) a = d $ const a

newtype State s = State s
newtype Props p = Props p

class FromContext eval context a m (eff :: # !) | eval context a m -> eff where
  fromContext :: eval -> context -> m eff a

instance stateFromContext :: TypeEquals state state2 => FromContext eval (ReactThis props state) (State state2) Eff (state::ReactState (read::Read)|eff) where
  fromContext _ this = State <<< to <$> readState this

instance propsFromContext :: TypeEquals props props2 => FromContext eval (ReactThis props state) (Props props2) Eff (props::ReactProps|eff) where
  fromContext _ this = Props <<< to <$> getProps this

instance thisFromContext :: (TypeEquals (ReactThis props state) (ReactThis props2 state2), Applicative (m eff)) => FromContext eval (ReactThis props state) (ReactThis props2 state2) m eff where
  fromContext _ this = pure $ to this
