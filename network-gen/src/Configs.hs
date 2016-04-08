module Configs where

import Control.Monad

import Env
import EnvGraph
import LaunchScript
import IfupScript
import Output


circle n = env ((listNames "m" n) ++ (listNames "l" n))
            ((map (\i -> br i ["m" ++ (show i), "l" ++ (show i)]) [1 .. n])
              ++ (map (\i -> br (10 * (i + 1) + ((i + 1) `mod` n + 1)) ["m" ++ (show $ i + 1), "m" ++ (show $ (i + 1) `mod` n + 1)]) [0 .. n - 1]))

listNames :: String -> Int -> [String]
listNames s n = map ((s ++) . show) [1 .. n]

fullMesh n = env ((listNames "m" n) ++ (listNames "l" n))
            ((map (\i -> br i ["m" ++ (show i), "l" ++ (show i)]) [1 .. n])
                ++ ([1 .. n] >>= (\i -> map (\j -> br (10 * i + j) ["m" ++ (show i), "m" ++ (show j)]) (filter (/=i) [1 .. n]))))

anycastTest1 = env ["mc1", "m2", "m3", "ms4", "m5", "ms6", "m7"]
                   [ br 12 ["mc1", "m2"]
                   , br 23 ["m2", "m3"]
                   , br 34 ["m3", "ms4"]
                   , br 45 ["ms4", "m5"]
                   , br 56 ["m5", "ms6"]
                   , br 67 ["ms6", "m7"]
                   , br 71 ["m7", "mc1"]
                   ]


