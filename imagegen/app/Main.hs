module Main where

import AnalyticAntialias (lineSegDefinition, trimHorizontal, trimVertical)
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
    "../doc/img/trimVertical.pdf"
    (dims (V2 400 400))
    trimVertical
  renderPGF
    "../doc/img/trimHorizontal.pdf"
    (dims (V2 400 400))
    trimHorizontal
