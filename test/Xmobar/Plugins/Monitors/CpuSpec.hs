module Xmobar.Plugins.Monitors.CpuSpec
  ( 
   spec, main
  ) where

import Test.Hspec
import Xmobar.Plugins.Monitors.Common
import Xmobar.Plugins.Monitors.Cpu
import Data.List

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "CPU Spec" $ do
    it "works with total template" $
      do let args = ["-L","3","-H","50","--normal","green","--high","red", "-t", "Cpu: <total>%"]
         cpuArgs <- getArguments args
         cpuValue <- runCpu cpuArgs
         cpuValue `shouldSatisfy` (\item -> "Cpu:" `isPrefixOf` item)
    it "works with bar template" $
      do let args = ["-L","3","-H","50","--normal","green","--high","red", "-t", "Cpu: <total>% <bar>"]
         cpuArgs <- getArguments args
         cpuValue <- runCpu cpuArgs
         cpuValue `shouldSatisfy` (all (`elem` ":#") . last . words)
    it "works with no icon pattern template" $
      do let args = ["-L","3","-H","50","--normal","green","--high","red", "-t", "Cpu: <total>% <bar>", "--", "--load-icon-pattern", "<icon=bright_%%.xpm/>"]
         cpuArgs <- getArguments args
         cpuValue <- runCpu cpuArgs
         cpuValue `shouldSatisfy` (\item -> not $ "<icon=bright_" `isInfixOf` cpuValue)
    it "works with icon pattern template" $
      do let args = ["-L","3","-H","50","--normal","green","--high","red", "-t", "Cpu: <total>% <bar> <ipat>", "--", "--load-icon-pattern", "<icon=bright_%%.xpm/>"]
         cpuArgs <- getArguments args
         cpuValue <- runCpu cpuArgs
         cpuValue `shouldSatisfy` (\item -> "<icon=bright_" `isInfixOf` cpuValue)
    it "works with other parameters in template" $
      do let args = ["-L","3","-H","50","--normal","green","--high","red", "-t", "Cpu: <user> <nice> <iowait>"]
         cpuArgs <- getArguments args
         cpuValue <- runCpu cpuArgs
         cpuValue `shouldSatisfy` (\item -> "Cpu:" `isPrefixOf` cpuValue)
