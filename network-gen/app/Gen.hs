module Gen where

import Control.Monad

import Draw
import Env
import LaunchScript
import IfupScript
import Scripts

main :: IO ()
main  =  putStrLn "Lol"


generate :: Env -> IO ()
generate  =  generateTo "./"

generateTo :: Directory -> Env -> IO ()
generateTo dir e  =  let store f = saveTo dir $ f e
                     in  store LaunchScript
                      >> store IfupScript 
                    
        

