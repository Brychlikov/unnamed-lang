{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Main where
-- import Interpreter
import Compiler 
import System.Environment (getArgs)
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad (void)
import Data.Text.IO
import Prelude hiding (readFile, writeFile)
import Compiler (compileProgWithStd)


main :: IO ()
main = do 
    args <- getArgs
    case args of
        [infile, outfile] -> do 
            src <- readFile infile 
            code <- compileProgWithStd src
            case code of 
                Left err -> print $ "Compilation error: " ++ err
                Right res -> writeFile outfile res

        _ -> print "Expected exactly two arguments"

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
