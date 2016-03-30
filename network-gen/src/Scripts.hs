module Scripts where

import Text.Printf
import Env

class Show s => Script s where
    defName :: s -> String
    
    saveTo :: Directory -> s -> IO ()
    saveTo path s  =  writeFile (norm path ++ defName s) $ show s
      where
        norm p | null p         =  "./"
               | last p == '/'  =  p
               | otherwise      =  p ++ "/"
          
    save :: s -> IO ()
    save = saveTo "./"   

type Directory  =  FilePath


-- some usefull functions

nodeCase :: (Node -> String) -> [Node] -> String
nodeCase nodeGen nodes  =  nodes >>= 
    printf "if [[ $name == '%s' ]]; then\n%sfi\n" <$> id <*> nodeGen
