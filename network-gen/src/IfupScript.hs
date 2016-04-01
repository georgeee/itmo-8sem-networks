{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module IfupScript where

import Env
import Text.RawString.QQ
import Text.Printf
import Output
import qualified Data.List as L
import Data.Maybe

newtype IfupScript  =  IfupScript Env

instance Show IfupScript where
    show (IfupScript e)  =  genIfup e

instance Output IfupScript where
    defName _  =  "ifaces.sh"


---------------- script start -----------------
genIfup :: Env -> String
genIfup e  =  [r|#!/bin/bash
name=$type$id

ifaces=()
bridges=()
indexes=()

|] ++ devs
----------------- script end ------------------

  where
    devs :: String
    devs = flip nodeCase (envNodes e) $ \node -> do
        (Bridge{..}, devNo) <- zip (holdingBridges node e) [envDevStartNo e ..]
        let devName = "ens" ++ show devNo
            nodeIdInBridge = (+1) $ fromJust $ L.findIndex ( == node) bridgeNodes
        printf "    ifaces+=(%s)\n    bridges+=(%d)\n    indexes+=(%d)\n"
            devName bridgeId nodeIdInBridge

