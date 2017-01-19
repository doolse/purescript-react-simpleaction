module SimpleAction.Dispatch (
  dispatch
, class Dispatchable
, effEval
, fromContext
, class FromContext
, Context(..)
, DispatchAff(..)
, DispatchEff(..)
, DispatchEffFn(..)
, DispatchEffFn2(..)
, DispatchEffFn3(..)
) where

import Prelude
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Function.Eff (EffFn1, EffFn2, EffFn3, mkEffFn1, mkEffFn2, mkEffFn3)
import Data.Maybe (Maybe, maybe)
import Type.Equality (class TypeEquals, to)

class Dispatchable eval context action m | eval -> context where
  dispatch :: eval -> context -> action -> m Unit

instance funcMaybeDispatch :: (Applicative m,  Dispatchable (action -> dispatchable) context dispatchable m)
  => Dispatchable (action -> dispatchable) context (Maybe action) m where
  dispatch f c ma = maybe (pure unit) (\a -> dispatch f c (f a)) ma

instance funcDispatch :: Dispatchable (action -> next) context next m => Dispatchable (action -> next) context action m where
  dispatch f c a = dispatch f c (f a)

instance readerTTest :: Dispatchable eval context (m a) n => Dispatchable eval context (ReaderT context m a) n where
  dispatch e c a = dispatch e c $ runReaderT a c

instance effDispacher :: Dispatchable eval context (Eff eff a) (Eff eff2) where
  dispatch _ _ = void <<< unsafeCoerceEff

instance affDispacher :: Dispatchable eval context (Aff eff a) (Aff eff2) where
  dispatch _ _ = void <<< unsafeCoerceAff

instance aff2EffDispacher :: Dispatchable eval context (Aff eff a) (Eff eff2) where
  dispatch e c = (dispatch e c) <<< launchAff

effEval ::  forall a context eff. (a -> ReaderT context (Eff eff) Unit) -> a -> ReaderT context (Eff eff) Unit
effEval = id

class FromContext eval context a m (eff :: # !) | eval context a m -> eff where
  fromContext :: eval -> context -> m eff a

newtype DispatchEff a = DispatchEff (forall ev eff. (ev -> a) -> ev -> Eff eff Unit)

newtype DispatchAff a = DispatchAff (forall ev eff. (ev -> a) -> ev -> Aff eff Unit)

newtype DispatchEffFn a = DispatchEffFn (forall ev eff. (ev -> a) -> EffFn1 eff ev Unit)

newtype DispatchEffFn2 a = DispatchEffFn2 (forall ev ev2 eff. (ev -> ev2 -> a) -> EffFn2 eff ev ev2 Unit)

newtype DispatchEffFn3 a = DispatchEffFn3 (forall ev ev2 ev3 eff. (ev -> ev2 -> ev3 -> a) -> EffFn3 eff ev ev2 ev3 Unit)

instance dispatchAffD :: (Applicative (m eff2), Dispatchable eval context action (Aff eff))
  => FromContext eval context (DispatchAff action) m eff2 where
  fromContext eval c = pure $ DispatchAff (\handle ev -> unsafeCoerceAff $ (dispatch eval c (handle ev)) :: Aff eff Unit)

instance dispatchEffD :: (Applicative (m eff2), Dispatchable eval context action (Eff eff))
  => FromContext eval context (DispatchEff action) m eff2 where
  fromContext eval c = pure $ DispatchEff (\handle ev -> unsafeCoerceEff $ (dispatch eval c (handle ev)) :: Eff eff Unit)

instance dispatchEffFnD :: (Applicative (m eff2), Dispatchable eval context action (Eff eff))
  => FromContext eval context (DispatchEffFn action) m eff2 where
  fromContext eval c = pure $ DispatchEffFn (\handle -> mkEffFn1 \ev -> unsafeCoerceEff $ (dispatch eval c (handle ev)) :: Eff eff Unit)

instance dispatchEffFn2D :: (Applicative (m eff2), Dispatchable eval context action (Eff eff))
  => FromContext eval context (DispatchEffFn2 action) m eff2 where
  fromContext eval c = pure $ DispatchEffFn2 (\handle -> mkEffFn2 \ev ev2 -> unsafeCoerceEff $ (dispatch eval c (handle ev ev2)) :: Eff eff Unit)

instance dispatchEffFn3D :: (Applicative (m eff2), Dispatchable eval context action (Eff eff))
  => FromContext eval context (DispatchEffFn3 action) m eff2 where
  fromContext eval c = pure $ DispatchEffFn3 (\handle -> mkEffFn3 \ev ev2 ev3 -> unsafeCoerceEff $ (dispatch eval c (handle ev ev2 ev3)) :: Eff eff Unit)

newtype Context a = Context a

instance contextFromContext :: (TypeEquals context context2, Applicative (m eff)) => FromContext eval context (Context context2) m eff where
  fromContext _ c = pure (Context $ to c)
