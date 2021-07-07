module Main where

import AnalyticAntialias (lineSegDefinition)
import Diagrams.Backend.PGF
import Diagrams.Prelude

main :: IO ()
main = do
  putStrLn "Rendering PDF diagrams"
  renderPGF
    "../doc/img/lineSegDefinition.pdf"
    (dims (V2 400 400))
    lineSegDefinition
