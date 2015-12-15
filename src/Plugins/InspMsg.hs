{-# LANGUAGE NoImplicitPrelude  #-}
module Plugins.InspMsg (InspMsg(..)) where

import Plugins (readFileSafe, tenthSeconds)

import Prelude (String, Ing, IO, Show(..),
               lines, length)
import Data.Array (listArray, (!))
import Control.Monad (forever, liftM)
import System.Random (randomRIO)

type Name = String
type File = String
type Rate = Int

data InspMsg = InspMsg Name File Rate
     deriving (Show)

instance Exec InspMsg where
  alias (InspMsg n _ _) = n
  start (InspMsg _ f r) = runInspMsg f r

runInspMsg :: File -> Rate -> (String -> IO ()) -> IO ()
runInspMsg file runRate callback = do
  fdata <- liftM lines (readFileSafe file)
  let len = length fdata
      listData = listArray (1, len) fdata
  forever $ do 
    index <- randomRIO (1, len)
    callback (listData ! index)
    tenthSeconds runRate
          
  
