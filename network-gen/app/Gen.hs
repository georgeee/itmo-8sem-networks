module Gen where

import Control.Monad

import Draw
import Env
import LaunchScript
import InitScript
import Scripts

main :: IO ()
main  =  putStrLn "Lol"


generate :: Env -> IO ()
generate  =  generateTo "./"

generateTo :: Directory -> Env -> IO ()
generateTo dir e  =  mapM_ (save dir . ( $ e)) [LaunchScript] 
        

