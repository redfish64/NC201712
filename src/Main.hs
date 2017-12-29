{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Network.Kademlia as K
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.Arrow (first)

data Person = Person {
                age :: Int
              , name :: String
              }
              deriving (Show,Read,Eq)

instance K.Serialize Person where
   toBS = C.pack . show
   fromBS bs =
       case (reads :: ReadS Person) . C.unpack $ bs of
           [] -> Left "Failed to parse Person."
           (result, rest):_ -> Right (result, C.pack rest)

newtype KademliaID = KademliaID B.ByteString
  deriving (Eq,Ord)

instance K.Serialize KademliaID where
   toBS (KademliaID bs)
       | B.length bs >= 5 = B.take 5 bs
       | otherwise        = error "KademliaID to short!"

   fromBS bs
       | B.length bs >= 5 = Right . first KademliaID . B.splitAt 5 $ bs
       | otherwise        = Left "ByteString too short!"

-- instance Eq KademliaID       
-- instance Ord KademliaID       

main :: IO ()
main = do
   -- Create the first instance, which will serve as the first node of the
   -- network
   firstInstance <- K.create 12345 . KademliaID . C.pack $ "hello" 

   -- Create a Node representing the first instance
   let firstNode = K.Node (K.Peer "192.168.43.103" 12345) . KademliaID . C.pack $ "hello"

   -- Create the second instance and make it join the network
   secondInstance <- K.create 12346 . KademliaID . C.pack $ "uAleu"
   joinResult <- K.joinNetwork secondInstance firstNode

   -- Make sure the joining was successful
   case joinResult of
        K.JoinSucces -> do
            -- Store an example value in the network
            let exampleValue = Person 25 "Alan Turing"
            K.store secondInstance (KademliaID . C.pack $ "raxqT") exampleValue

            let exampleValue = Person 26 "Alan Turings"
            K.store firstInstance (KademliaID . C.pack $ "raxqZ") exampleValue

            -- Look up the value and it's source
            Just (value, source) <- K.lookup firstInstance . KademliaID . C.pack $ "raxqT"

            print (name value)

        _ -> return ()

   -- Close the instances
   K.close firstInstance
   K.close secondInstance



