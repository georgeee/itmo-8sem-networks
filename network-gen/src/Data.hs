{-# LANGUAGE TupleSections #-}

module Data where

import qualified Data.Map as Map
import qualified Data.Int as I
import Control.Applicative
import qualified Data.Char as Char
import qualified Data.List as L
import Control.Monad
import Control.Monad.Trans

data Node = Node 
    { nodeType :: String
    , nodeId   :: I.Int8 
    } deriving (Eq)

data Bridge  =  Bridge 
    { inetEnabled :: InetEnabled
    , bridgeNodes :: [Node]
    } deriving (Show) 

type InetEnabled  =  Bool

data Env  =  Env 
    { envNodes :: [Node]
    , envBridges :: Map.Map Int Bridge
    } deriving (Show)

instance Show Node where
    show (Node t id)  =  t ++ show id


type NodeName  =  String

envBridgesList :: Env -> [Bridge]
envBridgesList  =  map snd . Map.toList . envBridges

readNode :: String -> Node
readNode s  =  
    let (t, r) = L.span Char.isAlpha s
        (i, r2)  = L.span Char.isDigit r        
    in  if null t || null i || not (null r2)
            then error $ "Illegal node name " ++ s 
            else Node t (read i)

env :: [NodeName] -> [(InetEnabled, [NodeName])] -> Env 
env nodes bridges  =  either error id $ checkEnv $ Env 
    { envNodes   = readNode <$> nodes
    , envBridges = Map.fromList $ zip [1..] $ makeBridge <$> bridges
    }
  where
    makeBridge (ie, nodes) = Bridge ie (readNode <$> nodes)
  
checkEnv :: Env -> Either String Env
checkEnv e@(Env nodes bridges)  =  fmap (const e) $ sequence $ do
    bridge <- snd <$> Map.assocs bridges
    subnode <- bridgeNodes bridge
    pure $ if subnode `elem` nodes
        then Right ()
        else Left $ "Node " ++ show subnode ++ " is not within node list"
