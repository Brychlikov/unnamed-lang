{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Main where
-- import Interpreter
import Compiler 
import System.Environment (getArgs)
import qualified Data.Text as T
import System.IO
import Shelly
import Data.Text (Text)
import Control.Monad (void)


main :: IO ()
main = do 
    return ()
    -- hSetBuffering stdout LineBuffering
    -- args <- getArgs
    -- out <- runJSCode "console.log('silent');"
    -- putStrLn $ T.unpack out
    -- case args of 
    --     [infile, outfile] -> do 
    --         src <- readFile infile 
    --         case compileSrc $ T.pack src  of 
    --             Right res -> do 
    --                 prelude <- readFile "std.js"
    --                 writeFile outfile (prelude ++ T.unpack res)
    --             Left err -> putStrLn err
    --     _ ->
    --         putStrLn "Two args required"
