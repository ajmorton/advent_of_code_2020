module Lib ( run ) where

import System.Environment ( getArgs )   
import System.IO ()  

import Day01 ( runDay01 )
import Day02 ( runDay02 )
import Day03 ( runDay03 )
import Day04 ( runDay04 )
import Day05 ( runDay05 )
import Day06 ( runDay06 )
      
run :: IO ()
run = do  
    day <- fmap (read . head) getArgs
    input <- readFile $ "data/" ++ show day ++ ".txt"
    runDay day input

runDay :: Int -> String -> IO ()    
runDay 1 inp = print $ runDay01 inp
runDay 2 inp = print $ runDay02 inp
runDay 3 inp = print $ runDay03 inp
runDay 4 inp = print $ runDay04 inp
runDay 5 inp = print $ runDay05 inp
runDay 6 inp = print $ runDay06 inp
