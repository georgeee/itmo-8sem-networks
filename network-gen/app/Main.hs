module Main where

import Control.Monad

import Env
import EnvGraph
import LaunchScript
import IfupScript
import Output

main :: IO ()
main  =  putStrLn "Lol"

saveTo' path e = do
  saveTo path $ LaunchScript e
  saveTo path $ IfupScript e
  saveTo path $ EnvGraph e
