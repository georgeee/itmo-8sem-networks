module Draw where

import Graphviz
import Env
import Text.Printf
import Control.Monad
import Control.Lens

instance Graphviz Env where
    buildGraph e  =  "graph environment {\n" ++ entries  ++ "}\n"     
      where
        entries = do
            entry <- nodesList (envNodes e) ++ edgesList (envEdges e)
            '\t' : entry ++ "\n"

        nodesList :: [Node] -> [String]
        nodesList = map $ join $ printf "%s [label=\"%s\"];"
        
        edgesList :: [(Node, Node)] -> [String]
        edgesList = map $ uncurry $ printf "%s -- %s;" 
        
            
