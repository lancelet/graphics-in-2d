module Main where

import AnalyticAntialias (lineSegDefinition, pqPrime)
import Diagrams.Backend.PGF
import Diagrams.Prelude

main :: IO ()
main = do
  putStrLn "Rendering PDF diagrams"
  renderPGF
    "../doc/img/lineSegDefinition.pdf"
    (dims (V2 400 400))
    lineSegDefinition
  renderPGF
    "../doc/img/pqPrime.pdf"
    (dims (V2 400 400))
    pqPrime
