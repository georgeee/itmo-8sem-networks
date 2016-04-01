{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Env where

import qualified Data.Int as I
import Control.Applicative
import qualified Data.Char as Char
import qualified Data.List as L
import Control.Monad
import Control.Monad.Trans
import Control.Lens
import Control.Arrow
import Text.Printf

data Node = Node 
    { nodeType       :: String
    , nodeId         :: I.Int8 
    } deriving (Eq, Ord)

data Bridge  =  Bridge 
    { bridgeInetEnabled :: InetEnabled
    , bridgeId    :: BridgeId
    , bridgeNodes :: [Node]
    } deriving (Show) 

data Env  =  Env 
    { envNodes      :: [Node]
    , envBridges    :: [Bridge]
    , envServers    :: [Node]
    , envClients    :: [Node]
    , envDevStartNo :: Int
    } deriving (Show)

type InetEnabled  =  Bool
type BridgeId  =  Int


instance Show Node where
    show  =  (++) <$> nodeType <*> show . nodeId

instance PrintfArg Node where
    formatArg n format  =  (++) $ case fmtChar format of
        's' -> show n
        'i' -> show $ nodeId n
        't' -> nodeType n 
        bad -> error $ "Bad format letter '" ++ bad : "' for data Node"

envEdges :: Env -> [(BridgeId, (Node, Node))]    
envEdges  =  (sequence . (bridgeId &&& (sequence . (head &&& drop 1) =<< ) 
                 . L.tails . bridgeNodes) =<< ) . envBridges

holdingBridges :: Node -> Env -> [Bridge]
holdingBridges n  =  filter (elem n . bridgeNodes) . envBridges

readNode :: String -> Node
readNode s  =  
    let (t, r) = L.span Char.isAlpha s
        (i, r2)  = L.span Char.isDigit r        
    in  if null t || null i || not (null r2)
            then error $ "Illegal node name " ++ s 
            else Node { nodeType = t 
                      , nodeId = (read i)
                      }

type NodeName  =  String
type ServerNodeName  =  String
type ClientNodeName  =  String
type DevStartNo  =  Int

extEnv :: [NodeName] -> DevStartNo -> [ServerNodeName] -> [ClientNodeName] -> [(InetEnabled, [NodeName])] -> Env 
extEnv nodes devNo servers clients bridges  =  either error id $ checkEnv $ Env 
    { envNodes      = readNode <$> nodes
    , envBridges    = zipWith (&) [1..] $ makeBridge <$> bridges
    , envServers    = readNode <$> servers
    , envClients    = readNode <$> clients
    , envDevStartNo = devNo
    }
  where
    makeBridge (ie, nodes) id = Bridge ie id (readNode <$> nodes)
  
env :: [NodeName] -> [[NodeName]] -> Env
env ns  =  extEnv ns 3 [] [] . map (False, )

checkEnv :: Env -> Either String Env
checkEnv e@Env{..}  =  e <$ do
    traverse checkNodeDefined usedNodes
    checkDups
  where
    usedNodes = (envBridges >>= bridgeNodes)
             ++ envServers
             ++ envClients
    
    checkNodeDefined n = unless (n `elem` envNodes) $ Left $ printf "Node %s is not within node list" n
    
    checkDups = let ns = L.sort envNodes
                    checkNeib (n1, n2) = when (n1 == n2) $ Left ("duplicate node: " ++ show n1)
                in  unless (null ns) $ void $ traverse checkNeib $ zip ns (tail ns)
            

