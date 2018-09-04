module UI.React where

import Prelude

import Control.Comonad (class Comonad)
import Data.Functor.Pairing (class Pairing)
import Effect (Effect)
import React as R
import UI (ComponentT, UI, explore) as UI

-- | A `ReactUI` is a `UI` whose action handlers run in the `Effect` monad, producing
-- | interfaces described by `ReactElement`s.
type ReactUI m = UI.UI Effect m R.ReactElement

-- | A `ReactComponent` is a comonad `w` describing a space of all future possible
-- | `ReactUI`s.
type ReactComponent w m =
  UI.ComponentT
    Effect
    w
    m
    R.ReactElement

-- | Explore a `ReactComponent` whose comonad and monad do form a `Pairing` by producing
-- | a `ReactClass` which can be rendered using React.
explore
  :: forall w m
   . Comonad w
  => Pairing m w
  => ReactComponent w m
  -> R.ReactClass {}
explore space = R.component "ComonadComponent" \this -> pure
  { state: { space }
  , render: do
      state <- R.getState this
      pure $ UI.explore (\s -> R.writeState this { space: s }) state.space
  }

-- | Instantiate a `ReactComponent` as a `ReactElement`
toReact :: forall w m. Comonad w => Pairing m w => ReactComponent w m -> R.ReactElement
toReact = flip R.createLeafElement {} <<< explore
