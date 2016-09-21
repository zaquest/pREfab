module Editor.Style where

import Prelude

-- | Style is a little bit special (orthogonal) to the rest of the `Editor`
-- types, so we don't put these types into `Editor.Types`
type StrokeStyle = String
type FillStyle = String

type LineStyle = { style :: StrokeStyle
                 , width :: Number }

type PointStyle = { style :: FillStyle
                  , radius :: Number }

type Style = { mainGrid :: LineStyle
             , subGrid :: LineStyle
             , subGridMinZoom :: Number -- ^ Don't show sub-cube grid when zoom is less than this value
             , point :: PointStyle
             , edge :: LineStyle
             , invalidFaceStyle :: FillStyle -- ^ Fill style to draw invalid face
             , drawOrigin :: Boolean -- ^ Should draw the origin
             , origin :: PointStyle }


rgb :: Int -> Int -> Int -> String
rgb r g b = "rgb(" <> show r <> "," <> show g <> "," <> show b <> ")"

rgba :: Int -> Int -> Int -> Number -> String
rgba r g b a = "rgba(" <> show r <> "," <> show g <> "," <> show b <> "," <> show a <> ")"

defaultStyle :: Style
defaultStyle = { mainGrid: { style: rgb 0 0 0, width: 1.0 }
               , subGrid: { style: rgb 0 0 0, width: 0.25 }
               , subGridMinZoom: 0.5
               , point: { style: rgb 0 255 0, radius: 5.0 }
               , edge: { style: rgb 0 255 0, width: 2.0 }
               , invalidFaceStyle: rgba 255 0 0 0.3
               , drawOrigin: true
               , origin: { style: rgb 255 0 0, radius: 5.0 } }
