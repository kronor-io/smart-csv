{-# OPTIONS_GHC -main-is Main.main #-}

module Main (main) where

import RIO
import SmartCsvRunner.ThreadManager (startAllServices)


main :: IO ()
main = startAllServices
