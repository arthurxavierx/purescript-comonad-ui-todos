module UI.React where

import Prelude

import Control.Comonad (class Comonad)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Functor.Pairing (class Pairing)
import React as R
import UI (ComponentT, UI, explore) as UI

type ReactEff eff =
  Eff
    ( state :: R.ReactState R.ReadWrite
    , props :: R.ReactProps
    , refs :: R.ReactRefs (read :: R.Read)
    | eff)

type ReactUI eff m = UI.UI (ReactEff eff) m R.ReactElement

type ReactComponent eff w m =
  UI.ComponentT
    (ReactEff eff)
    w
    m
    R.ReactElement

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
      pure $ UI.explore (void <<< liftEff <<< R.writeState this) state
