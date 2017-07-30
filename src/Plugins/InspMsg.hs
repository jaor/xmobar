{-# LANGUAGE NoImplicitPrelude  #-}

{-
A plugin to read arbitrary messages from a specified file and write the out
in a random order (randomRIO). To be used with inspirational messages (hence name),
fun facts, historical quotes...
-}


module Plugins.InspMsg (InspMsg(..)) where

import Plugins (Exec(..), tenthSeconds)

import Prelude (Int, IO, String, Show(..), Read(..),
               length, ($))
--Using Text and Text.IO reduces overhead significantly for large files
import Data.Text (unpack, lines)
--The use of readFile instead of readFileSafe is justified for large files
import Data.Text.IO (readFile)
import Data.Array (listArray, (!))
import Control.Monad (forever, liftM)
import System.Random (randomRIO)

type Name = String
type File = String
type Rate = Int

--alias, file to read from & rate (10th's of second)
data InspMsg = InspMsg Name File Rate
     deriving (Read, Show)

instance Exec InspMsg where
  alias (InspMsg n _ _) = n
  start (InspMsg _ f r) = runInspMsg f r

runInspMsg :: File -> Rate -> (String -> IO ()) ->  ()
runInspMsg file runRate callback = do
  --read all data from file and split into lines
  fdata <- liftM lines (readFile file)
  --Put data into array for O(1) accesss
  let len = length fdata
      listData = listArray (1, len) fdata
  --generate index, write data, wait. Repeat
  forever $ do 
    index <- randomRIO (1, len)
    callback $ unpack $ listData ! index
    tenthSeconds runRate
          
  
