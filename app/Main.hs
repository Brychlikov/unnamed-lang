{-# LANGUAGE OverloadedStrings #-}
module Main where
import Interpreter

main :: IO ()
main = do 
    res <- runWithType "let f x y = x in f 10 "
    print res
    -- putStrLn ":)"
