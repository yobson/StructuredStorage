{-# LANGUAGE DataKinds, PolyKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module System.IO.StructuredStorage
( Capture
, File
, (:<|>)(..)
, (:>)
, FileMonad
, readF
, writeF
, runOps
, liftF
) where

import Control.Applicative
import GHC.TypeLits
import Data.Proxy
import Control.Monad.State.Lazy

import System.IO.StructuredStorage.Internal.FileMonad

data a :<|> b = a :<|> b
infixr 8 :<|>

data (a :: k) :> (b :: *)
infixr 9 :>

data Capture (a :: *)

data File (a :: *)

class HasRead api where
  type Reader (api :: *) :: *
  readAt :: Proxy api -> FilePath -> Reader api

class HasWrite api where
  type Writer (api :: *) :: *
  writeAt :: Proxy api -> FilePath -> Writer api

class HasFile api where
  type Client (api :: *) :: *
  clientAt :: Proxy api -> FilePath -> Client api

-----------------------
-- HasFile Instances --
-----------------------

instance (HasFile a, HasFile b) => HasFile (a :<|> b) where
  type Client (a :<|> b) = Client a :<|> Client b

  clientAt _ file = clientAt (Proxy @a) file :<|> clientAt (Proxy @b) file

instance (Show a, HasFile r) => HasFile (Capture a :> r) where
  type Client (Capture a :> r) = a -> Client r

  clientAt _ file x = clientAt (Proxy @r) $ file ++ ('/':(show x))

instance (KnownSymbol s, HasFile r) => HasFile (s :> r) where
  type Client ((s :: Symbol) :> r) = Client r

  clientAt _ file = clientAt (Proxy @r) $ file ++ ('/':symb)
      where symb = symbolVal (Proxy @s)

instance (Read f) => HasFile (File f) where
  type Client (File f) = FileMonad f ()

  clientAt _ file = put file


--------------------
-- Read Instances --
--------------------

instance (HasRead a, HasRead b) => HasRead (a :<|> b) where
  type Reader (a :<|> b) = Reader a :<|> Reader b

  readAt _ file = readAt (Proxy @a) file :<|> readAt (Proxy @b) file

instance (Show a, HasRead r) => HasRead (Capture a :> r) where
  type Reader (Capture a :> r) = a -> Reader r

  readAt _ file x = readAt (Proxy @r) $ file ++ ('/':(show x))

instance (KnownSymbol s, HasRead r) => HasRead (s :> r) where
  type Reader ((s :: Symbol) :> r) = Reader r

  readAt _ file = readAt (Proxy @r) $ file ++ ('/':symb)
      where symb = symbolVal (Proxy @s)

instance (Read f) => HasRead (File f) where
  type Reader (File f) = IO f

  readAt _ file = readFile file >>= return . read

---------------------
-- Write Instances --
---------------------

instance (HasWrite a, HasWrite b) => HasWrite (a :<|> b) where
  type Writer (a :<|> b) = Writer a :<|> Writer b

  writeAt _ file = writeAt (Proxy @a) file :<|> writeAt (Proxy @b) file

instance (Show a, HasWrite r) => HasWrite (Capture a :> r) where
  type Writer (Capture a :> r) = a -> Writer r

  writeAt _ file x = writeAt (Proxy @r) $ file ++ ('/':(show x))

instance (KnownSymbol s, HasWrite r) => HasWrite (s :> r) where
  type Writer ((s :: Symbol) :> r) = Writer r

  writeAt _ file = writeAt (Proxy @r) $ file ++ ('/':symb)
      where symb = symbolVal (Proxy @s)

instance (Show f) => HasWrite (File f) where
  type Writer (File f) = f -> IO ()

  writeAt _ file cont = writeFile file (show cont)
