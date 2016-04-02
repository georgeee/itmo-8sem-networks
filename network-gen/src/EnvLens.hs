{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}

module EnvLens 
    ( nodes
    , ofType
    , bridges
    , servers
    , clients
    , startDevNo
    , bid      
    , ie 
    , ($>)            
    , (<:=)
    , (<++=)
    , newEnv
    , newBridge
    , renumBridges
    ) 
where

import Control.Lens
import Env                              
import qualified Data.List as L
import Data.Int
import Control.Monad.State
                                     
makeLensesFor 
    [ ("envNodes"      , "_envNodes" )
    , ("envServers"    , "_servers"  )
    , ("envClients"    , "_clients"  )
    , ("envBridges"    , "bridges"   )
    , ("envDevStartNo" , "startDevNo")
    ] ''Env                              

makeLensesFor
    [ ("bridgeNodes",       "_bridgeNodes")
    , ("bridgeId"   ,       "bid"         )
    , ("bridgeInetEnabled", "ie"          )
    ] ''Bridge

class NodeLike n where
    asNode :: Iso' n Node
    asName :: Iso' n NodeName

instance NodeLike Node where
    asNode = id
    asName = iso show readNode

instance NodeLike [Char] where
    asNode = from asName 
    asName = id

class WithNodes ns where
    nodes :: NodeLike n => Lens' ns [n]

instance WithNodes Env where
    nodes  =  _envNodes . mapping (from asNode)

instance WithNodes Bridge where
    nodes  =  _bridgeNodes . mapping (from asNode)

newEnv :: State Env () -> Env      
newEnv  =  either error id . checkEnv . flip execState Env 
    { envNodes = []
    , envServers = []
    , envClients = []
    , envBridges = []
    , envDevStartNo = 4
    }

newBridge :: State Bridge () -> Bridge
newBridge  =  flip execState Bridge
    { bridgeNodes = []
    , bridgeId = 0
    , bridgeInetEnabled = True                              
    }

servers :: NodeLike n => Lens' Env [n]
servers  =  _servers . mapping (from asNode)

clients :: NodeLike n => Lens' Env [n]
clients  =  _clients . mapping (from asNode)


ofType :: String -> Lens' [Node] [Int8]
ofType t  =  lens get set 
  where
    get = map nodeId . filter (( == t) . nodeType)
    set s b = (filter (( /= t) . nodeType) s) ++ (Node t <$> b) 

renumBridges :: State Env ()
renumBridges  =  modify $ bridges %~ zipWith (set bid) [1..]

infixr 4 <++=
(<++=) :: MonadState s m => ASetter s s [a] [a] -> [a] -> m ()
s <++= e  =  s %= ( ++ e)

infixr 4 <:=
(<:=) :: MonadState s m => ASetter s s [a] [a] -> a -> m ()
s <:= e  =  s <++= [e]

infixr 9 $>
($>) :: (a -> b) -> a -> b
($>)  =  ($)
