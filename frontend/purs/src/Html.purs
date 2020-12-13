module Html
  ( classList
  ) where

import Prelude
import Data.Array (filter)
import Data.Foldable (intercalate)

classList :: Array { className :: String, active :: Boolean } -> String
classList =
  intercalate " "
    <<< map (_.className)
    <<< filter (_.active)
