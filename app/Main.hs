module Main (main) where

import Myque.Github.Cli (runCli)
import System.Environment (getArgs)
import System.Exit (exitWith)

main :: IO ()
main = getArgs >>= runCli >>= exitWith
