module Main where

import Qwu.Api.Server

import Network.Wai
import Network.Wai.Handler.Warp

main :: IO ()
main = run 8081 app

-- main :: IO ()
-- main = putStrLn "hasdfd"
