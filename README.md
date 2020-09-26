# StructuredStorage
A servant inspired api for treating a structured file system as an api

## Motivations
At work, I am going to be creatinng a tool that uses the file system for storage as opposed to any database.
I was worried about hand crafting the file paths. How could I be sure that I got them right and continue to
in the future? I thought of how servant solves the exact same problem and stole the idea.

## Library Usage
```haskell
import Data.Proxy
import System.IO.Structrued.Storage
import System.IO.Structrued.Storage.Flatten

-- We first define an api
type FileAPI = "tmp" :/ "Files" :/ (  Capture Int :/ Capture String :/ File String
                                 :<|> Capture String :/ File String
                                   )

-- We make a proxy of our api

api :: Proxy API
api = Proxy

-- We then define the client functions

getFstFile :: Int -> String -> FileMonad String ()
getSndFile :: String -> FileMonad String ()
getFstFile :<|> getSndFile = clientIn (flatten api) "/"


fileOps = getFstFile 10 "Hello.txt" >>= writeF "Hello World"

main = runOps fileOps >> return ()

-- This program will put "Hello World" into "/tmp/10/Hello.txt"

```

There is still a lot of work to do!
