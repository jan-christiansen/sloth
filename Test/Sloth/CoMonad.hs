module Test.Sloth.CoMonad
  (
    CoPointed(..), CoMonad(..), (<<=)
  ) where


import Control.Monad.Identity
import Control.Monad.Writer


class CoPointed f where
  extract :: f a -> a

instance CoPointed Identity where
  extract (Identity x) = x


class (Functor w,CoPointed w) => CoMonad w where
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f  = fmap f . duplicate

(<<=) :: CoMonad w => w a -> (w a -> b) -> w b
m <<= f = extend f m 

instance CoMonad Identity where
  duplicate = Identity

instance CoPointed m => CoPointed (WriterT w m) where
  extract = fst . extract . runWriterT
