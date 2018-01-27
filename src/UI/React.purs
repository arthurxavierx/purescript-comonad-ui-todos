module UI.React where

import Prelude

import Control.Comonad (class Comonad)
import Control.Monad.Eff (Eff)
import Data.Functor.Pairing (class Pairing)
import React as R
import UI (ComponentT, UI, explore) as UI

type ReactEff eff =
  Eff
    ( state :: R.ReactState R.ReadWrite
    , props :: R.ReactProps
    , refs :: R.ReactRefs (read :: R.Read)
    | eff)

-- | A `ReactUI` is a `UI` whose action handlers run in the `Eff` monad, producing
-- | interfaces described by `ReactElement`s.
type ReactUI eff m = UI.UI (ReactEff eff) m R.ReactElement

-- | A `ReactComponent` is a comonad `w` describing a space of all future possible
-- | `ReactUI`s.
type ReactComponent eff w m =
  UI.ComponentT
    (ReactEff eff)
    w
    m
    R.ReactElement

-- | Explore a `ReactComponent` whose comonad and monad do form a `Pairing` by producing
-- | a `ReactClass` which can be rendered using React.
explore
  :: forall eff w m props
   . Comonad w
  => Pairing m w
  => ReactComponent eff w m
  -> R.ReactClass props
explore space = R.createClass (R.spec space render)
  where
    render this = do
      state <- R.readState this
      pure $ UI.explore (void <<< R.writeState this) state

-- | Instantiate a `ReactComponent` as a `ReactElement`
toReact :: forall w m eff. Comonad w => Pairing m w => ReactComponent eff w m -> R.ReactElement
toReact = flip R.createFactory {} <<< explore
