module Editor.Editor
     ( Editor
     , mkEditor
     ) where

import Graphics.Canvas (Context2D)

import Grid (CGrid)
import Data.Polygon (Poly2)
import Editor.Style (Style, defaultStyle)
import Editor.View (View, mkView)
import Editor.WorkArea (WorkArea, mkWorkArea)

type Editor = { context :: Context2D
              , style :: Style
              , view :: View
              , workArea :: WorkArea }

mkEditor :: Context2D -> Number -> Number -> Poly2 CGrid -> Editor
mkEditor ctx w h poly = { context: ctx
                        , style: defaultStyle
                        , view: mkView w h
                        , workArea: mkWorkArea poly }
