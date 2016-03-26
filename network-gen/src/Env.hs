{-# LANGUAGE TupleSections #-}

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
    { nodeType :: String
    , nodeId   :: I.Int8 
    } deriving (Eq)

data Bridge  =  Bridge 
    { inetEnabled :: InetEnabled
    , bridgeId    :: Int
    , bridgeNodes :: [Node]
    } deriving (Show) 

type InetEnabled  =  Bool

data Env  =  Env 
    { envNodes :: [Node]
    , envBridges :: [Bridge]
    } deriving (Show)

instance Show Node where
    show (Node t id)  =  t ++ show id

instance PrintfArg Node where
    formatArg n format  =  (++) $ case fmtChar format of
        's' -> show n
        'i' -> show $ nodeId n
        't' -> nodeType n 
        bad -> error $ "Bad format letter '" ++ bad : "' for data Node"

type NodeName  =  String

envBridgesList :: Env -> [Bridge]
envBridgesList  =  envBridges

envEdges :: Env -> [(Node, Node)]    
envEdges  =  ((sequence . (head &&& drop 1) =<< ) 
                 . L.tails . bridgeNodes =<< ) . envBridges

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
    , envBridges = zipWith (&) [1..] $ makeBridge <$> bridges
    }
  where
    makeBridge (ie, nodes) id = Bridge ie id (readNode <$> nodes)
  
checkEnv :: Env -> Either String Env
checkEnv e@(Env nodes bridges)  =  fmap (const e) $ sequence $ do
    bridge <- bridges
    subnode <- bridgeNodes bridge
    pure $ if subnode `elem` nodes
        then Right ()
        else Left $ "Node " ++ show subnode ++ " is not within node list"
