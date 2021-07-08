{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module AnalyticAntialias where

import Data.Colour.Names (black, lightgrey, purple)
import Data.Functor ((<&>))
import Data.Monoid (Any)
import Diagrams
  ( Path,
    QDiagram,
    Renderable,
    V2,
    alignedText,
    circle,
    dashingN,
    fc,
    fcA,
    fontSize,
    frame,
    fromVertices,
    lc,
    local,
    lw,
    medium,
    none,
    p2,
    rect,
    strokeLocLoop,
    thick,
    translateX,
    translateY,
    (#),
  )
import Diagrams.Prelude (withOpacity)
import Diagrams.TwoD.Arrow (arrowBetween)
import Diagrams.TwoD.Text (Text)

type Dia =
  forall b.
  ( Renderable (Path V2 Float) b,
    Renderable (Text Float) b
  ) =>
  QDiagram b V2 Float Any

lineSegDefinition :: Dia
lineSegDefinition =
  mconcat
    [ -- axes
      arrowBetween (p2 (-0.2, 0)) (p2 (6, 0)) # lw thick,
      arrowBetween (p2 (0, -0.2)) (p2 (0, 5)) # lw thick,
      alignedText 0.5 1.0 "$x$"
        # translateX 6.0
        # translateY (-0.2)
        # fontSize (local 0.5),
      alignedText 1.0 0.5 "$y$"
        # translateX (-0.2)
        # translateY 5.0
        # fontSize (local 0.5),
      -- pixel rectangle
      rect 4.0 3.0 # translateX 3.0 # translateY 2.5 # lw medium,
      -- line
      circle 0.1 # translateX 0.5 # translateY 2.5 # fc black,
      circle 0.1 # translateX 3.5 # translateY 4.5 # fc black,
      fromVertices [p2 (0.5, 2.5), p2 (3.5, 4.5)] # lw thick,
      alignedText 0.5 1.0 "$\\mathbf{p}$"
        # translateX 0.5
        # translateY (2.5 - 0.3)
        # fontSize (local 0.5),
      alignedText 0.0 0.5 "$\\mathbf{q}$"
        # translateX (3.5 + 0.3)
        # translateY 4.5
        # fontSize (local 0.5),
      -- region
      fromVertices
        [ p2 (1, 1),
          p2 (1, 17.0 / 6.0),
          p2 (11.0 / 4.0, 4.0),
          p2 (3.5, 4.0),
          p2 (3.5, 1.0),
          p2 (1, 1)
        ]
        # strokeLocLoop
        # lw none
        # fcA (purple `withOpacity` 0.2),
      -- u and v labels
      mconcat
        [ fromVertices [p2 (1, -0.2), p2 (1, 1)], -- u0
          fromVertices [p2 (5, -0.2), p2 (5, 1)], -- u1
          fromVertices [p2 (-0.2, 1), p2 (1, 1)], -- v0
          fromVertices [p2 (-0.2, 4), p2 (1, 4)] -- v1
        ]
        # dashingN [0.02, 0.02] 0,
      alignedText 0.5 1.0 "$u_0$"
        # translateX 1.0
        # translateY (-0.4)
        # fontSize (local 0.5),
      alignedText 0.5 1.0 "$u_1$"
        # translateX 5
        # translateY (-0.4)
        # fontSize (local 0.5),
      alignedText 1.0 0.5 "$v_0$"
        # translateX (-0.4)
        # translateY 1
        # fontSize (local 0.5),
      alignedText 1.0 0.5 "$v_1$"
        # translateX (-0.4)
        # translateY 4
        # fontSize (local 0.5)
        -- helper grid
        --helperGrid 6 5
    ]
    # frame 0.8

trimVertical :: Dia
trimVertical =
  mconcat
    [ -- axes
      arrowBetween (p2 (-0.2, 0)) (p2 (6, 0)) # lw thick,
      arrowBetween (p2 (0, -0.2)) (p2 (0, 5)) # lw thick,
      alignedText 0.5 1.0 "$x$"
        # translateX 6.0
        # translateY (-0.2)
        # fontSize (local 0.5),
      alignedText 1.0 0.5 "$y$"
        # translateX (-0.2)
        # translateY 5.0
        # fontSize (local 0.5),
      -- pixel rectangle
      rect 4.0 3.0 # translateX 3.0 # translateY 2.5 # lw medium,
      -- line
      circle 0.1 # translateX (-1) # translateY 2.5 # fc black,
      circle 0.1 # translateX 6.5 # translateY 3.5 # fc black,
      circle 0.1 # translateX 1 # translateY (83.0 / 30.0) # fc black,
      circle 0.1 # translateX 5 # translateY (99.0 / 30.0) # fc black,
      fromVertices [p2 (-1, 2.5), p2 (1, 83.0 / 30.0)]
        # lw thick
        # dashingN [0.01, 0.01] 0,
      fromVertices [p2 (5, 99.0 / 30.0), p2 (6.5, 3.5)]
        # lw thick
        # dashingN [0.01, 0.01] 0,
      fromVertices [p2 (1, 83.0 / 30.0), p2 (5.0, 99.0 / 30.0)] # lw thick,
      alignedText 0.5 1 "$\\mathbf{p}$"
        # translateX (-1.0)
        # translateY (2.5 - 0.3)
        # fontSize (local 0.5),
      alignedText 0.5 1 "$\\mathbf{q}$"
        # translateX 6.5
        # translateY (3.5 - 0.3)
        # fontSize (local 0.5),
      alignedText 0 1 "$\\mathbf{p}_1$"
        # translateX (1 + 0.2)
        # translateY (83.0 / 30.0 - 0.2)
        # fontSize (local 0.5),
      alignedText 1 1 "$\\mathbf{q}_1$"
        # translateX (5 - 0.2)
        # translateY (99.0 / 30.0 - 0.2)
        # fontSize (local 0.5),
      -- region
      fromVertices
        [ p2 (1, 1),
          p2 (1, 83.0 / 30.0),
          p2 (5, 99.0 / 30.0),
          p2 (5, 1),
          p2 (1, 1)
        ]
        # strokeLocLoop
        # lw none
        # fcA (purple `withOpacity` 0.2),
      -- u and v labels
      mconcat
        [ fromVertices [p2 (1, -0.2), p2 (1, 1)], -- u0
          fromVertices [p2 (5, -0.2), p2 (5, 1)], -- u1
          fromVertices [p2 (-0.2, 1), p2 (1, 1)], -- v0
          fromVertices [p2 (-0.2, 4), p2 (1, 4)] -- v1
        ]
        # dashingN [0.02, 0.02] 0,
      alignedText 0.5 1.0 "$u_0$"
        # translateX 1.0
        # translateY (-0.4)
        # fontSize (local 0.5),
      alignedText 0.5 1.0 "$u_1$"
        # translateX 5
        # translateY (-0.4)
        # fontSize (local 0.5),
      alignedText 1.0 0.5 "$v_0$"
        # translateX (-0.4)
        # translateY 1
        # fontSize (local 0.5),
      alignedText 1.0 0.5 "$v_1$"
        # translateX (-0.4)
        # translateY 4
        # fontSize (local 0.5)
        -- helper grid
        --helperGrid 6 5
    ]
    # frame 0.8

trimHorizontal :: Dia
trimHorizontal =
  mconcat
    [ -- axes
      arrowBetween (p2 (-0.2, 0)) (p2 (6, 0)) # lw thick,
      arrowBetween (p2 (0, -0.2)) (p2 (0, 5)) # lw thick,
      alignedText 0.5 1.0 "$x$"
        # translateX 6.0
        # translateY (-0.2)
        # fontSize (local 0.5),
      alignedText 1.0 0.5 "$y$"
        # translateX (-0.2)
        # translateY 5.0
        # fontSize (local 0.5),
      -- pixel rectangle
      rect 4.0 3.0 # translateX 3.0 # translateY 2.5 # lw medium,
      -- line
      circle 0.1 # translateX 2 # translateY 3 # fc black,
      circle 0.1 # translateX 3 # translateY (-1) # fc black,
      circle 0.1 # translateX 2.5 # translateY 1 # fc black,
      fromVertices [p2 (3, -1), p2 (2.5, 1)]
        # lw thick
        # dashingN [0.01, 0.01] 0,
      fromVertices [p2 (2.5, 1), p2 (2, 3)] # lw thick,
      alignedText 0 0.5 "$\\mathbf{b},\\mathbf{p}_1$"
        # translateX (2 + 0.3)
        # translateY 3
        # fontSize (local 0.5),
      alignedText 0 0.5 "$\\mathbf{a},\\mathbf{q}_1$"
        # translateX (3 + 0.3)
        # translateY (-1)
        # fontSize (local 0.5),
      alignedText 0 0 "$\\mathbf{a}_1$"
        # translateX (2.5 + 0.2)
        # translateY (1 + 0.2)
        # fontSize (local 0.5),
      -- region
      fromVertices
        [ p2 (2, 1),
          p2 (2, 3),
          p2 (2.5, 1),
          p2 (2, 1)
        ]
        # strokeLocLoop
        # lw none
        # fcA (purple `withOpacity` 0.2),
      -- u and v labels
      mconcat
        [ fromVertices [p2 (-0.2, 1), p2 (1, 1)], -- v0
          fromVertices [p2 (-0.2, 4), p2 (1, 4)] -- v1
        ]
        # dashingN [0.02, 0.02] 0,
      alignedText 1.0 0.5 "$v_0$"
        # translateX (-0.4)
        # translateY 1
        # fontSize (local 0.5),
      alignedText 1.0 0.5 "$v_1$"
        # translateX (-0.4)
        # translateY 4
        # fontSize (local 0.5)
        -- helper grid
        -- helperGrid 6 5
    ]
    # frame 0.8

helperGrid :: Int -> Int -> Dia
helperGrid nw nh = (hLines <> vLines) # lc lightgrey
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
