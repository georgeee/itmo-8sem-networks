{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module IfupScript where

import Env
import Text.RawString.QQ
import Text.Printf
import Scripts
import qualified Data.List as L
import Data.Maybe

newtype IfupScript  =  IfupScript Env

instance Show IfupScript where
    show (IfupScript e)  =  genIfup e  

instance Script IfupScript where
    defName _  =  "ifup.sh"


---------------- script start -----------------
genIfup :: Env -> String
genIfup e  =  [r|#!/bin/bash
type = %type%
id = %id%
name = $type$id

|] ++ devs  
----------------- script end ------------------

  where
    devs :: String
    devs = flip nodeCase (envNodes e) $ \node -> do
        (Bridge{..}, devNo) <- zip (holdingBridges node e) [envDevStartNo e ..]
        let devName = "ens" ++ show devNo
            nodeIdInBridge = (+1) $ fromJust $ L.findIndex ( == node) bridgeNodes 
        printf "    ensure_dev_up %s\n    ifconfig %s 192.168.%d.%d\n"  
            devName devName bridgeId nodeIdInBridge
           
