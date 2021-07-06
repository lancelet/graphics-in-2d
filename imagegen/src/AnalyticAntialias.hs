{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module AnalyticAntialias where

import Data.Colour.Names (grey, lightgrey)
import Data.Functor ((<&>))
import Data.Monoid (Any)
import Diagrams
  ( Path,
    QDiagram,
    Renderable,
    V2,
    circle,
    fromVertices,
    lc,
    p2,
    (#),
  )

type Dia = forall b. (Renderable (Path V2 Float) b) => QDiagram b V2 Float Any

test :: Dia
test = helperGrid 10 10 # lc grey <> circle 0.5

helperGrid :: Int -> Int -> Dia
helperGrid nw nh = hLines <> vLines # lc lightgrey
  where
    h, w :: Float
    h = fromIntegral nh
    w = fromIntegral nw

    hLines :: Dia
    hLines =
      mconcat $
        [0 .. nh]
          <&> fromIntegral
          <&> \y -> fromVertices [p2 (0, y), p2 (w, y)]

    vLines :: Dia
    vLines =
      mconcat $
        [0 .. nw]
          <&> fromIntegral
          <&> \x -> fromVertices [p2 (x, 0), p2 (x, h)]
