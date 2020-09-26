{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.IO.StructuredStorage.Internal.FileMonad
( FileMonad(..)
, readF
, writeF
, runOps
, liftF
) where


import Control.Monad.State.Lazy

newtype FileMonad f a = FileMonad (StateT FilePath IO a)
  deriving (Functor, Applicative, Monad, MonadState FilePath)

--data Mode = RO | RW | WO

runOps :: FileMonad f a -> IO a
runOps  (FileMonad s) = runStateT s [] >>= return . fst

liftF = FileMonad . lift

readF :: (Read f) => FileMonad f f
readF = get >>= liftF . readFile >>= return . read

writeF :: (Show f) => f -> FileMonad f ()
writeF s = get >>= liftF . (flip writeFile (show s))
