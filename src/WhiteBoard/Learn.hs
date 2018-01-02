module Learn where

import Data.Text

data WBMonad

class WBObj v where
  key :: v -> Text
  action :: v -> WBMonad
  
