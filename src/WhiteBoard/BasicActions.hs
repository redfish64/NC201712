module WhiteBoard.BasicActions where

import WhiteBoard.Types
import Data.Text
import Data.Typeable

-- -- actions and types for common scenarios

-- showToUser :: (WBObj x, Show x) => x -> WBMonad ()
-- showToUser x = undefined --modifyObj "AllErrors" (++ (show x))

-- data Error = Error {
--   msg :: Text
--   } deriving (Show,Read,Typeable,Eq)

-- instance WBObj Error where
--   key e = unpack $ msg e
--   action e = showToUser e


