module Data.Functor.Pairing where

import Prelude

import Control.Comonad (class Comonad)
import Control.Comonad.Cofree (Cofree, explore)
import Control.Comonad.Cofree.Trans (CofreeT, coresume)
import Control.Comonad.Env (EnvT(..))
import Control.Comonad.Store (StoreT(..))
import Control.Comonad.Traced (TracedT(..))
import Control.Monad.Free (Free)
import Control.Monad.Free.Trans (FreeT, resume)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (StateT(StateT))
import Control.Monad.Writer (WriterT(..))
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct(..))
import Data.Functor.Product (Product(..))
import Data.Identity (Identity(..))
import Data.Tuple (Tuple(Tuple))

class Pairing f g | f -> g, g -> f where
  pair :: forall a b c. (a -> b -> c) -> f a -> g b -> c

zap :: forall f g a b. Pairing f g => f (a -> b) -> g a -> b
zap = pair ($)

pairFlipped :: forall a b c f g. Pairing f g => (a -> b -> c) -> g a -> f b -> c
pairFlipped f ga fb = pair (flip f) fb ga

instance pairIdentity :: Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance pairTupleFunction :: Pairing (Tuple a) ((->) a) where
  pair f (Tuple a b) g = f b (g a)

instance pairProductCoproduct :: (Pairing f1 g1, Pairing f2 g2) => Pairing (Product f1 f2) (Coproduct g1 g2) where
  pair f (Product (Tuple f1 f2)) (Coproduct e) =
    case e of
      Left g1 -> pair f f1 g1
      Right g2 -> pair f f2 g2

instance pairStateStore :: Pairing f g => Pairing (StateT s f) (StoreT s g) where
  pair f (StateT state) (StoreT (Tuple wf s)) =
    pair (\(Tuple a s1) f1 -> f a (f1 s1)) (state s) wf

instance pairWriterTraced :: Pairing f g => Pairing (WriterT w f) (TracedT w g) where
  pair f (WriterT writer) (TracedT gf) =
    pair (\(Tuple a w) f1 -> f a (f1 w)) writer gf

instance pairReaderEnv :: Pairing f g => Pairing (ReaderT e f) (EnvT e g) where
  pair f (ReaderT reader) (EnvT (Tuple e gb)) = pair f (reader e) gb

instance pairFreeCofree :: (Functor f, Functor g, Pairing f g) => Pairing (Free f) (Cofree g) where
  pair f = explore zap <<< map f

instance pairFreeTCofreeT :: (Functor f, Functor g, MonadRec m, Comonad w, Pairing f g, Pairing m w) => Pairing (FreeT f m) (CofreeT g w) where
  pair f m w = go (step m w)
    where
      go (Tuple mf wc) =
        case pair Tuple mf wc of
          Tuple (Left a) (Tuple b _) -> f a b
          Tuple (Right ff) (Tuple _ tail) -> go (pair step ff tail)

      step a b = Tuple (resume a) (coresume b)
