module EnvGraph where

import Env
import Output
import Text.Printf
import Control.Monad
import Control.Lens

newtype EnvGraph  =  EnvGraph Env

instance Show EnvGraph where
    show (EnvGraph e)  =  "graph environment {\n" ++ entries ++ "}\n"     
      where
        entries = do
            entry <- (nodesList <$> envNodes e) ++ (edgesList <$> envEdges e)
            '\t' : entry ++ "\n"

        nodesList :: Node -> String
        nodesList = liftM3 (printf "%s [label=\"%s\", color=\"%s\"];") id id nodeColor
       
        edgesList :: (BridgeId, (Node, Node)) -> String
        edgesList (bid, (n1, n2)) = printf "%s -- %s [label=\"%d\"];" n1 n2 bid
        
        nodeColor n 
            | n `elem` envClients e 
           && n `elem` envServers e  =  "cyan3"
            | n `elem` envClients e  =  "green"
            | n `elem` envServers e  =  "blue"
            | otherwise              =  "black"
        
instance Output EnvGraph where
    defName _  =  "graph.dot" 
           
