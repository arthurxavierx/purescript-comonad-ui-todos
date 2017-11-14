module Data.Machine.Moore where

import Prelude

import Control.Comonad (class Comonad)
import Control.Comonad.Cofree.Trans (CofreeT, unfoldCofreeT)
import Control.Comonad.Trans.Class (class ComonadTrans)
import Control.Extend (class Extend)
import Control.Monad.Free.Trans (FreeT, liftFreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (class MonadTrans)
import Data.Functor.Pairing (class Pairing, pair)
import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..))

newtype MooreT i w a = MooreT (CofreeT ((->) i) w a)
type Moore i = MooreT i Identity

unfoldMooreT :: forall w s i a. Comonad w => (s -> w (Tuple a (i -> s))) -> s -> MooreT i w a
unfoldMooreT f = MooreT <<< unfoldCofreeT f

unfoldMoore :: forall s i a. (s -> Tuple a (i -> s)) -> s -> Moore i a
unfoldMoore f = MooreT <<< unfoldCofreeT (Identity <<< f)

derive newtype instance functorMooreT :: Functor w => Functor (MooreT i w)
derive newtype instance extendMooreT :: Comonad w => Extend (MooreT i w)
derive newtype instance comonadMooreT :: Comonad w => Comonad (MooreT i w)
derive newtype instance comonadTransMooreT :: ComonadTrans (MooreT i)

--------------------------------------------------------------------------------

newtype ComooreT i m a = ComooreT (FreeT (Tuple i) m a)
type Comoore i = ComooreT i Identity

liftComooreT :: forall m i a. Monad m => i -> a -> ComooreT i m a
liftComooreT input = ComooreT <<< liftFreeT <<< Tuple input

action :: forall m i. Monad m => i -> ComooreT i m Unit
action = flip liftComooreT unit

derive newtype instance functorComooreT :: Functor m => Functor (ComooreT i m)
derive newtype instance applyComooreT :: Monad m => Apply (ComooreT i m)
derive newtype instance applicativeComooreT :: Monad m => Applicative (ComooreT i m)
derive newtype instance bindComooreT :: Monad m => Bind (ComooreT i m)
derive newtype instance monadComooreT :: Monad m => Monad (ComooreT i m)
derive newtype instance monadTransComooreT :: MonadTrans (ComooreT i)
derive newtype instance monadRecComooreT :: Monad m => MonadRec (ComooreT i m)

instance pairComooreTMooreT :: (MonadRec m, Comonad w, Pairing m w) => Pairing (ComooreT i m) (MooreT i w) where
  pair f (ComooreT m) (MooreT w) = pair f m w
