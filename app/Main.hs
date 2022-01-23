{-# LANGUAGE OverloadedStrings #-}
module Main where
import Interpreter
import Compiler 
import System.Environment (getArgs)
import qualified Data.Text as T

main :: IO ()
main = do 
    args <- getArgs
    case args of 
        [infile, outfile] -> do 
            src <- readFile infile 
            case compileSrc $ T.pack src  of 
                Right res -> do 
                    prelude <- readFile "std.js"
                    writeFile outfile (prelude ++ (T.unpack res)  )
                Left err -> putStrLn err
        _ ->
            putStrLn "Two args required"
