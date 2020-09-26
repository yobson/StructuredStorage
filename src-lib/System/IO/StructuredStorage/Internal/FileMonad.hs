{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.IO.StructuredStorage.Internal.FileMonad
( FileMonad(..)
, readF
, writeF
, runOps
, liftF
, FileType
, toFile
, fromFile
) where


import Control.Monad.State.Lazy
import System.Directory
import System.FilePath

import qualified Data.ByteString.Lazy as B

newtype FileMonad a = FileMonad (StateT FilePath IO a)
  deriving (Functor, Applicative, Monad, MonadState FilePath)

--data Mode = RO | RW | WO

class FileType a where
  toFile   :: a -> B.ByteString
  fromFile :: B.ByteString -> Maybe a

runOps :: FileMonad a -> IO a
runOps  (FileMonad s) = runStateT s [] >>= return . fst

liftF = FileMonad . lift

readF :: (FileType f) => FileMonad (Maybe f)
readF = get >>= liftF . B.readFile >>= return . fromFile

writeF :: (FileType f) => f -> FileMonad ()
writeF s = do
    fp <- get
    let dir = dropFileName fp
    liftF $ createDirectoryIfMissing True dir
    liftF $ B.writeFile fp (toFile s)
