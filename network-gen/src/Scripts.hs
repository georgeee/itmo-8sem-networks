module Scripts where

class Show s => Script s where
    defName :: s -> String
    
    save :: FilePath -> s -> IO ()
    save path s  =  writeFile (norm path ++ defName s) $ show s
      where
        norm p | null p         =  "./"
               | last p == '/'  =  p
               | otherwise      =  p ++ "/"
             

type Directory  =  FilePath
