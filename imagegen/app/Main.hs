module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG

b1 :: Diagram B
b1 = square 20 # lw 0.002 # fc blue

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  renderSVG "../doc/img/square.svg" (dims (V2 400 400)) b1
