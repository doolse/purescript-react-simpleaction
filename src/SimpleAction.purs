module SimpleAction where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import React (ReactClass, ReactElement, ReactProps, ReactRefs, ReactState, ReactThis, Read, Render, createClass, createElement, getProps, readState, spec)
import React.DOM (div, text)
import React.DOM.Props (onClick)
import Prelude hiding (div)

class Renderer renderer eval props state eff | renderer -> eff where
  createRenderer :: eval -> renderer -> Render props state eff

class Dispatchable e a eout | e -> a, a -> eout where
  doDispatch :: e -> a -> eout

newtype Dispatcher t a = Dispatcher (forall ev. (ev -> a) -> ev -> t)

instance dispatchableEff :: Dispatchable (a -> Eff eff Unit) a (Eff eff Unit) where
  doDispatch = ($)

data FullState p s = FullState p s
newtype StateOnly s = StateOnly s
newtype PropsOnly p = PropsOnly p

class RenderState fullstate props state eff | fullstate -> props state, fullstate -> eff where
  prepState :: ReactThis props state -> Eff (props :: ReactProps, refs :: ReactRefs (), state :: ReactState (read::Read) | eff) fullstate

instance fullState :: RenderState (FullState props state) props state eff where
  prepState t = do
    props <- getProps t
    state <- readState t
    pure $ FullState props state
--
-- instance fullStateChildren :: RenderState (FullState {props::p, state::s, children::Array ReactElement}) p s (Array ReactElement) (props::ReactProps, state::ReactState (read::Read) | eff) where
--   prepState t = do
--     props <- getProps t
--     state <- readState t
--     children <- getChildren t
--     pure $ FullState {props,state,children}
--
--
instance stateOnly :: RenderState (StateOnly s) p s eff where
  prepState t = StateOnly <$> readState t

instance propsOnly :: RenderState (PropsOnly p) p s eff where
  prepState t = PropsOnly <$> getProps t

instance pureStateReact :: RenderState fs p s eff => Renderer (fs -> ReactElement) e p s eff where
  createRenderer _ f t = do
    s <- prepState t
    pure $ f s

instance dispatchStateReact :: (RenderState fs props state eff, Dispatchable eval a eout) =>
  Renderer (fs -> (Dispatcher eout a) -> ReactElement) eval props state eff where
  createRenderer e f t = do
    s <- prepState t
    pure $ f s $ Dispatcher (\handle ev -> doDispatch e (handle ev))



createComponent :: forall p s r e eff. Renderer r e p s eff => s -> r -> e -> (ReactClass p)
createComponent s r e = createClass $ (spec s $ createRenderer e r)

data Action = DoSomething Int

type State = {hi::String}

main :: Array ReactElement -> ReactElement
main = createElement (createComponent {hi:"Hello"} render eval) {yo:"asd"}
  where
    -- render :: forall eff. (FullState {yo::String} {hi::String}) -> Dispatcher (Eff (props::ReactProps,refs::ReactRefs (read::Read), state::ReactState (read::Read,write::Write)|eff) Unit) Action -> ReactElement
    render (FullState p s) (Dispatcher d) = div [onClick $ d \_ -> DoSomething 1] $ [ text s.hi, text p.yo ]
    eval (DoSomething i) = logShow i
