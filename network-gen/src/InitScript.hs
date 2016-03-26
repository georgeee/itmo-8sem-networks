{-# LANGUAGE QuasiQuotes #-}

module InitScript where

import Env
import Text.RawString.QQ


genInitStub :: Env -> String
genInitStub e  =  [r|    

|]
