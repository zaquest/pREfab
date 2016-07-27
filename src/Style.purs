module Style where

import Prelude

rgb :: Int -> Int -> Int -> String
rgb r g b = "rgb(" <> show r <> "," <> show g <> "," <> show b <> ")"

rgba :: Int -> Int -> Int -> Number -> String
rgba r g b a = "rgba(" <> show r <> "," <> show g <> "," <> show b <> "," <> show a <> ")"

type StrokeStyle = String
type FillStyle = String

type LineStyle = { style :: StrokeStyle
                 , width :: Number }

type PointStyle = { style :: FillStyle
                  , radius :: Number }

type Style = { mainGrid :: LineStyle
             , subGrid :: LineStyle
             , subGridSize :: Number -- ^ Absolute size of sub-cube grid cell
             , subGridMinZoom :: Number -- ^ Don't show sub-cube grid when zoom is less than this value
             , point :: PointStyle
             , edge :: LineStyle
             , invalidFaceStyle :: FillStyle -- ^ Fill style to draw invalid face
             , drawOrigin :: Boolean -- ^ Should draw the origin
             , origin :: PointStyle }

defaultStyle :: Style
defaultStyle = { mainGrid: { style: rgb 0 0 0, width: 1.0 }
               , subGrid: { style: rgb 0 0 0, width: 0.25 }
               , subGridSize: 8.0
               , subGridMinZoom: 0.5
               , point: { style: rgb 0 255 0, radius: 5.0 }
               , edge: { style: rgb 0 255 0, width: 1.0 }
               , invalidFaceStyle: rgba 255 0 0 0.3
               , drawOrigin: true
               , origin: { style: rgb 255 0 0, radius: 5.0 } }
