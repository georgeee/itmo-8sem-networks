import Env
import EnvLens
import Control.Lens
import Control.Monad
import Data.Int
import Data.Foldable (for_)

simpleNet :: Env
simpleNet  =  newEnv $ do
    nodes      .= ["m1", "m2", "s1"]
    startDevNo .= 5

    bridges <:= newBridge $> do
        nodes    .= ["m1", "m2"]
        ie       .= False

    bridges <:= newBridge $> do
        nodes    .= ["m1", "s1"]

    bridges %= renum

cycleNet :: Int8 -> Env
cycleNet m  =  newEnv $ do
    nodes.ofType "m" .= [1..m]
    nodes.ofType "s" .= [1..m]
    
    for_ [1..m] $ \k -> 
        bridges <:= newBridge $> do
            nodes.ofType "m" .= [k]
            nodes.ofType "s" .= [k]
            bid .= fromIntegral k
    
    for_ (circle m) $ \(n1, n2) -> 
        bridges <:= newBridge $> do
            nodes.ofType "m" .= [n1, n2]
            bid .= fromIntegral (n1 * 10 + n2)
    
  where
    circle :: (Enum a, Integral a) => a -> [(a, a)]
    circle k  =  [(x, mod x k + 1) | x <- [1..k]]
        

