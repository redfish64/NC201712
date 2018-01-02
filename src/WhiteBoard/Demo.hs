{-# LANGUAGE OverloadedStrings #-}
module Main where

import WhiteBoard.Types as WT
import WhiteBoard.Core as C
import Prelude
import qualified Data.Text as T
import qualified Data.Text.IO as I
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as BL(pack,unpack,ByteString(..))
import Data.TCache.Defs(Serializable(..))
import Control.Concurrent
import Control.Monad.Reader
import System.IO

main :: IO ()
main =
  do
    argsStr <- getArgs
    _test argsStr
    return ()

_test :: [String] -> IO (WBConf Key MyObj)
_test argsStr =
  do
    contentsArray <- mapM I.readFile argsStr
    wbc <- createWBConf myActionFunc
    let objsToAdd = (zip (fmap KFile argsStr)
                       (fmap (\(fn,fc) -> MOFile fn fc) (zip argsStr contentsArray)))

    putStrLn $ "objsToAdd: "++(show objsToAdd)

    startWhiteBoard wbc

    runWBMonad wbc $ addAnchorObjects objsToAdd
    finishWhiteBoard wbc

    return wbc
    

data File = File {
  contents :: T.Text
  } deriving (Show, Read, Eq, Ord)

instance Serializable File where
  serialize= BL.pack . show
  deserialize= read . BL.unpack

instance WBObj File 

data Key = KFile FilePath deriving (Show, Read, Eq)

data MyObj = MOFile FilePath T.Text | MOSym { name :: T.Text, typeText :: T.Text, valText :: T.Text } deriving (Show,Read,Eq, Ord)

instance WBObj MyObj where

instance Serializable MyObj where
  serialize= BL.pack . show
  deserialize= read . BL.unpack

instance Serializable Key where
  serialize= BL.pack . show
  deserialize= read . BL.unpack
  
instance Keyable Key where

myActionFunc :: WBIMonad Key MyObj ()
myActionFunc = do
  (k,o) <- ask
  case k of
    (KFile _) -> return ()


-- instance WBObj File where
--   key x = [pack . fp $ x]
--   action o = (parseFile (contents o))

-- parseFile :: Text -> WBMonad ()
-- parseFile txt = do
--   case parseSyntax txt of
--     Left errors -> mapM storeObject errors
--     Right syms -> mapM storeObject syms
--   return ()


-- parseSyntax :: Text -> Either [Error] [Sym]
-- parseSyntax = --undefined
--   return Left [Error "foo"]


-- data Sym

-- instance WBObj Sym where
--   key = undefined
--   action = undefined

