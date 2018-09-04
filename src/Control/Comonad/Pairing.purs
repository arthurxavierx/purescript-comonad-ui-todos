module Control.Comonad.Pairing where

import Prelude

import Control.Comonad (class Comonad, duplicate)
import Data.Functor.Pairing (class Pairing, pair, pairFlipped)

select :: forall m w a b. Pairing m w => w a -> m b -> b
select = pairFlipped (const identity)

move :: forall m w a. Comonad w => Pairing m w => w a -> m Unit -> w a
move w m = pair (const identity) m (duplicate w)
