{-# LANGUAGE DataKinds, PolyKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, UndecidableInstances #-}

module System.IO.StructuredStorage
( Capture
, File
, Directory
, (:<|>)(..)
, (:/)
, FileMonad
, readF
, writeF
, runOps
, liftF
, FileType
, toFile
, fromFile
, Capturable
, toPath
, clientAt
) where

import Control.Applicative
import GHC.TypeLits
import Data.Proxy
import Control.Monad.State.Lazy
import Network.URI.Encode
import Data.Time.LocalTime

import System.IO.StructuredStorage.Internal.FileMonad

data a :<|> b = a :<|> b
infixr 8 :<|>

data (a :: k) :/ (b :: *)
infixr 9 :/

data Capture (a :: *)

data File (s :: Symbol) (a :: *)

data Directory

class HasFile api where
  type Client (api :: *) :: *
  clientAt :: Proxy api -> FilePath -> Client api

-----------------------
-- HasFile Instances --
-----------------------

instance (HasFile a, HasFile b) => HasFile (a :<|> b) where
  type Client (a :<|> b) = Client a :<|> Client b

  clientAt _ file = clientAt (Proxy @a) file :<|> clientAt (Proxy @b) file

instance (Capturable a, HasFile r) => HasFile (Capture a :/ r) where
  type Client (Capture a :/ r) = a -> Client r

  clientAt _ file x = clientAt (Proxy @r) $ file ++ ('/':(toPath x))

instance (KnownSymbol s, HasFile r) => HasFile (s :/ r) where
  type Client ((s :: Symbol) :/ r) = Client r

  clientAt _ file = clientAt (Proxy @r) $ file ++ ('/':symb)
      where symb = symbolVal (Proxy @s)

instance (FileType f, KnownSymbol s) => HasFile (File s f) where
  type Client (File s f) = FileMonad ()

  clientAt _ file = put $ file ++ ('/':symb)
    where symb = symbolVal (Proxy @s)

-------------------------------------
-- Converting Types to path strings --
--------------------------------------

class Capturable a where
  toPath :: a -> String

instance Capturable Int where
  toPath = show

instance Capturable Integer where
  toPath = show

instance Capturable String where
  toPath = encode

instance (Capturable a, Capturable b) => Capturable (a,b) where
  toPath (x,y) = concat [toPath x, "-", toPath y]

instance (Capturable a, Capturable b, Capturable c) => Capturable (a,b,c) where
  toPath (x,y,z) = concat [toPath x, "-", toPath y, "-", toPath z]

instance Capturable ZonedTime where
  toPath = filter (/=' ') . show

