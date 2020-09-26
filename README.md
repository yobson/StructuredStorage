# StructuredStorage
A servant inspired api for treating a structured file system as an api

## Motivations
At work, I am going to be creatinng a tool that uses the file system for storage as opposed to any database.
I was worried about hand crafting the file paths. How could I be sure that I got them right and continue to
in the future? I thought of how servant solves the exact same problem and stole the idea.

## Library Usage
Here is a comprehensive example:
```haskell
{-# LANGUAGE DeriveGeneric #-}     -- Used by Aeson
{-# LANGUAGE FlexibleInstances #-} -- Needed for line 28
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import GHC.Generics

-- Needed to use library
import Data.Proxy

-- These are exported by the library
import System.IO.StructuredStorage
import System.IO.StructuredStorage.Flatten

-- We start by making a Type and JSON encoding

data Person = Person { name :: String
                     , age  :: Int
                     } deriving (Generic, Show)

instance ToJSON Person where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Person

-- We are going to store a list of people in a file
-- for this we need to define FileType for [People]

instance FileType [Person] where
  toFile   = encode
  fromFile = decode

data Department = IT | Development

instance Capturable Department where
  toPath IT          = "IT"
  toPath Development = "Development"

-- We can now use the Department type in a filepath

-- Next we can define our API
type FileAPI = "tmp" :/ (  "Current" :/ Capture Department :/ File "people.json" [Person]
                      :<|> "Old"     :/ File "people.json" [Person]
                        )

-- In the api defined above, we two folders, tmp/Current and tmp/Old
-- In both current we have /IT and /Development, both of which can
-- contain people.json. We can have a People.json in tmp/old

-- We now define the functions to access the files
currentPeople :: Department -> FileMonad ()
oldPeople     :: FileMonad ()

-- We use proxy to pass our type to the next function
api :: Proxy FileAPI
api = Proxy

-- We magically fill the functions defined above and prefix our api with /
currentPeople :<|> oldPeople = clientAt (flatten api) "/"

someIOOperations :: FileMonad (Maybe [Person])
someIOOperations = do
  let peopleIT  = take 2 $ repeat $ Person {name="Foo", age = 666}
  let peopleDev = take 3 $ repeat $ Person {name="Bar", age = 666}
  currentPeople IT
  writeF peopleIT
  currentPeople Development
  writeF peopleDev
  currentPeople IT
  readF

main :: IO ()
main = runOps someIOOperations >>= print
```

There is still a lot of work to do!
