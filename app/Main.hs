module Main where

import Server
import Network.Wai
import Network.Wai.Handler.Warp

main :: IO ()
main = run 8081 app
