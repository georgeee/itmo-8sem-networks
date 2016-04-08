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
    let (i', t') = L.span Char.isDigit $ L.reverse s
        i = L.reverse i'
        t = L.reverse t'
    in  if null t || null i
            then error $ "Illegal node name " ++ s
            else Node { nodeType = t
                      , nodeId = (read i)
                      }

type NodeName  =  String
type ServerNodeName  =  String
type ClientNodeName  =  String
type DevStartNo  =  Int

env0 :: [NodeName] -> DevStartNo -> [ServerNodeName] -> [ClientNodeName] -> [Bridge] -> Env
env0 nodes devNo servers clients bridges  =  either error id $ checkEnv $ Env
    { envNodes      = readNode   <$> nodes
    , envBridges    = bridges
    , envServers    = readNode   <$> servers
    , envClients    = readNode   <$> clients
    , envDevStartNo = devNo
    }

env1 :: [NodeName] -> [ServerNodeName] -> [ClientNodeName] -> [Bridge] -> Env
env1 ns  =  env0 ns 4

env :: [NodeName] -> [Bridge] -> Env
env ns  =  env1 ns [] []

br1 :: BridgeId -> InetEnabled -> [NodeName] -> Bridge
br1 id ie ns  =  Bridge ie id $ map readNode ns

br :: BridgeId -> [NodeName] -> Bridge
br id ns  =  Bridge True id $ map readNode ns

checkEnv :: Env -> Either String Env
checkEnv e@Env{..}  =  e <$ do
    traverse checkNodeDefined usedNodes
    maybe (Right ()) (Left . printf "Duplicate node: %s") $ findDups envNodes
    maybe (Right ()) (Left . printf "Duplicate bridge id: %d") $ findDups $ bridgeId <$> envBridges
  where
    usedNodes = (envBridges >>= bridgeNodes)
             ++ envServers
             ++ envClients

    checkNodeDefined n = unless (n `elem` envNodes) $ Left $ printf "Node %s is not within node list" n


findDups :: Ord a => [a] -> Maybe a
findDups l = let ns = L.sort l
                 checkNeib (n1, n2) = when (n1 == n2) $ Left n1
             in  either Just (const Nothing) $ unless (null ns)
               $ void $ traverse checkNeib $ zip ns (tail ns)


