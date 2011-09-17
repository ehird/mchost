module Main where

import MC.Protocol

main :: IO ()
main = print (CKeepAlive 42)
