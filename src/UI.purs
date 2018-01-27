module UI where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Pairing (move)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Functor.Pairing (class Pairing)
import Data.Identity (Identity)

-- | A `Handler` is an event handler running in a `base` monad for actions of type `action`.
type Handler base action = base action -> base Unit

-- | A `UI` is a function which outputs a description of type `a` of an interface given
-- | a way to handle the `action`s dispatched by this interface through a `Handler`.
type UI base action a = Handler base action -> a

-- | A component is a comonad `w` representing the space of all possible future `UI`s.
-- | Through the use of `Pairing`s, one can `pair` this comonad `w` with a monad `m` for
-- | moving around in this space and, thus, modifying the state of the component.
-- |
-- | In this way, a `ComponentT` is a comonad `w` full of future `UI`s whose `Handler`s
-- | handle (in a `base` monad) actions of type `m Unit` dispatched by an interface of
-- | type `a`.
type ComponentT base w m a = w (UI base (m Unit) a)

type Component w m a = ComponentT Identity w m a

-- | Given a way of writing/updating the component's state within a `base` monad and
-- | a current or initial `ComponentT` (a space of interfaces), to `explore` a component
-- | means to get the current interface out of the component's comonad by wiring up the
-- | action handler with the `write` function supplied.
-- |
-- | This is a generic function for exploring the space of states defined by a comonad.
-- | Specific UI renderers must define their own specific derivations to effectively
-- | create the stateful components.
explore
  :: forall base w m a
   . Comonad w
  => Monad base
  => Pairing m w
  => (ComponentT base w m a -> base Unit)
  -> ComponentT base w m a
  -> a
explore write space = extract space send
  where
    send :: Handler base (m Unit)
    send base = base >>= \m -> write (move space m)

-- | Given a way of transforming actions of a child `UI` into actions of a parent `UI`,
-- | `liftUI` produces a natural transformation of child `UI`s into parent `UI`s.
liftUI
  :: forall base action actionp
   . Monad base
  => (action -> actionp)
  -> UI base action ~> UI base actionp
liftUI liftm ui = \send -> ui \m -> send (m >>= pure <<< liftm)

-- | Allow for transforming child `UI`s into parent `UI`s if the parent action type is a
-- | monad transformer and the child action type is a monad.
liftUIT
  :: forall base m mp a
   . Monad base
  => MonadTrans mp
  => Monad m
  => UI base (m a) ~> UI base (mp m a)
liftUIT = liftUI lift

-- | Produce a natural transformation of child `UI`s into parent `UI`s given a way of
-- | transforming effectful actions of the child into effectful actions of the parent.
liftUIEff
  :: forall base action actionp
   . Monad base
  => (base action -> base actionp)
  -> UI base action ~> UI base actionp
liftUIEff liftm ui = \send -> ui (send <<< liftm)

-- | Embed a child component into a parent component given a natural transformation from
-- | the child's monad into the parent's monad.
liftComponent
  :: forall base w m mp
   . Functor w
  => Monad base
  => m ~> mp
  -> ComponentT base w m ~> ComponentT base w mp
liftComponent liftm = map (liftUI liftm)

-- | Embed a child component into a parent one if the parent monad is a monad transformer.
liftComponentT
  :: forall base w m mp
   . Functor w
  => Monad base
  => MonadTrans mp
  => Monad m
  => ComponentT base w m ~> ComponentT base w (mp m)
liftComponentT = map liftUIT

-- | Embed a child component into a parent one given a way of transforming monadic
-- | actions of the child within a `base` monad into monadic actions of the parent within
-- | a `base` monad.
liftComponentEff
  :: forall base w m mp
   . Functor w
  => Monad base
  => (forall a. base (m a) -> base (mp a))
  -> ComponentT base w m ~> ComponentT base w mp
liftComponentEff liftm = map (liftUIEff liftm)

-- | Allow for the execution of monadic actions in the `base` monad (with access to the
-- | current state of the component) in response to every user action within a
-- | `ComponentT`.
-- |
-- | This function is a comonadic combinator.
effect
  :: forall base w m a
   . Monad base
  => Comonad w
  => Pairing m w
  => (ComponentT base w m a -> base Unit)
  -> ComponentT base w m a
  -> UI base (m Unit) a
effect eff component send = extract component \base -> send do
  action <- base
  eff (move component action)
  pure action

-- | Allow for the execution of monadic actions in the `base` monad in response to every
-- | user action within a `ComponentT`. The handling function has access to the current
-- | state of the component as well as to the dispatched action to which it responds.
-- |
-- | This function is a comonadic combinator.
effect'
  :: forall base w m a
   . Monad base
  => Comonad w
  => Pairing m w
  => (ComponentT base w m a -> m Unit -> base Unit)
  -> ComponentT base w m a
  -> UI base (m Unit) a
effect' eff component send = extract component \base -> send do
  action <- base
  eff (move component action) action
  pure action
