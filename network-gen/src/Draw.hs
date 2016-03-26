module Draw where

import Graphviz
import Data
import qualified Data.List as L
import Text.Printf
import Control.Monad
import qualified Data.Map as Map
import Control.Lens
import Control.Arrow

instance Graphviz Env where
    buildGraph e  =  "graph environment {\n" ++ entries  ++ "}\n"     
      where
        entries = do
            entry <- nodesList (envNodes e) ++ edgesList (envBridgesList e)
            '\t' : entry ++ "\n"

        nodesList :: [Node] -> [String]
        nodesList = map $ join (printf "%s [label=\"%s\"];") . show
        
        edgesList :: [Bridge] -> [String]
        edgesList = (=<<) $ \(Bridge _ nodes) -> 
            L.tails nodes >>= map printEdge . sequence . (head &&& drop 1)
        
        printEdge (n1, n2) = show n1 ++ " -- " ++ show n2 ++ ";"
            
            
