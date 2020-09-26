{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module System.IO.StructuredStorage.Flatten
( Flat
, flatten
) where

import Data.Proxy

import System.IO.StructuredStorage

type family Flatten (api :: k) :: k where
  Flatten ((a :: k) :> (b :<|> c)) = Flatten (a :> b) :<|> Flatten (a :> c)
  Flatten ((a :: k) :> b)          = Flatten a :> Flatten b
  Flatten (a :<|> b)               = Flatten a :<|> Flatten b
  Flatten a                        = a

type Reassoc api = ReassocBranch api '[]

type family ReassocBranch (currentAPI :: *) (otherEndpoints :: [*]) where
  ReassocBranch (a :<|> b)        rest = ReassocBranch a (b ': rest)
  ReassocBranch a                  '[] = a
  ReassocBranch a          (b ': rest) = a :<|> ReassocBranch b rest

type Flat api = Reassoc (Flatten api)

flatten :: Proxy api -> Proxy (Flat api)
flatten Proxy = Proxy
