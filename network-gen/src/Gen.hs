module Gen
    ( module Env
    , module EnvGraph
    , module LaunchScript
    , module IfupScript
    , module Output
    , module Configs
    , generate
    , generateTo
    )
where

import Control.Monad

import Env
import EnvGraph
import LaunchScript
import IfupScript
import Output
import Configs

main :: IO ()
main  =  putStrLn "Lol"


generate :: Env -> IO ()
generate  =  generateTo "./"

generateTo :: Directory -> Env -> IO ()
generateTo dir e  =  let storeAs f = saveTo dir $ f e
                     in  storeAs LaunchScript
                     >>  storeAs IfupScript
                     >>  storeAs EnvGraph



