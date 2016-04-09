module Configs where

import Control.Monad

import Env
import EnvGraph
import LaunchScript
import IfupScript
import Output
import Control.Lens
import EnvLens
import Data.Int (Int8)
import Data.Foldable (for_)

circle n = env ((listNames "m" n) ++ (listNames "l" n))
            ((map (\i -> br i ["m" ++ (show i), "l" ++ (show i)]) [1 .. n])
              ++ (map (\i -> br (10 * (i + 1) + ((i + 1) `mod` n + 1)) ["m" ++ (show $ i + 1), "m" ++ (show $ (i + 1) `mod` n + 1)]) [0 .. n - 1]))

listNames :: String -> Int8 -> [String]
listNames s n = map ((s ++) . show) [1 .. n]

fullMesh' n = env ((listNames "m" n) ++ (listNames "l" n))
            ((map (\i -> br i ["m" ++ (show i), "l" ++ (show i)]) [1 .. n])
                ++ ([1 .. n] >>= (\i -> map (\j -> br (10 * i + j) ["m" ++ (show i), "m" ++ (show j)]) (filter (/=i) [1 .. n]))))

fullMesh :: Int8 -> Env
fullMesh m  =  newEnv $ do
    nodes.ofType "m" .= [1..m]
    nodes.ofType "l" .= [1..m]
    bridges <:= newBridge $> do
        nodes.ofType "m" .= [1..m]
        bid .= 0
    for_ [1..m] $ \i -> 
        bridges <:= newBridge $> do
            nodes .= [Node "m" i, Node "l" i]
            bid .= i

anycastTest1 = env ["mc1", "m2", "m3", "ms4", "m5", "ms6", "m7"]
                   [ br 12 ["mc1", "m2"]
                   , br 23 ["m2", "m3"]
                   , br 34 ["m3", "ms4"]
                   , br 45 ["ms4", "m5"]
                   , br 56 ["m5", "ms6"]
                   , br 67 ["ms6", "m7"]
                   , br 71 ["m7", "mc1"]
                   ]

anycastTest2 = env [ "mc1", "m2", "m3", "m4", "ms5", "m6", "ms7"]
                   [ br 12 ["mc1", "m2"]
                   , br 14 ["mc1", "m4"]
                   , br 16 ["mc1", "m6"]
                   , br 23 ["m2", "m3"]
                   , br 34 ["m3", "m4"]
                   , br 45 ["m4", "ms5"]
                   , br 67 ["m6", "ms7"]
                   ]

anycastTest3 = env [ "mc1", "m2", "ms3", "ms4", "ms5", "m6", "m7", "ms8"]
                   [ br 12 ["mc1", "m2"]
                   , br 23 ["m2", "ms3"]
                   , br 24 ["m2", "ms4"]
                   , br 25 ["m2", "ms5"]
                   , br 16 ["mc1", "m6"]
                   , br 67 ["m6", "m7"]
                   , br 78 ["m7", "ms8"]
                   ]


                   