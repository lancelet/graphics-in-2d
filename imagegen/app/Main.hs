module Main where

import AnalyticAntialias (test)
import Diagrams.Backend.PGF
import Diagrams.Prelude

b1 :: Diagram B
b1 =
  mconcat
    [ text "$\\frac{x^2}{y^2}$",
      square 20 # lw 0.002 # fc blue
    ]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  renderPGF "../doc/img/square.pdf" (dims (V2 400 400)) b1
