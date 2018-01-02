module NomicCoin.Types where

newtype Error = Error { unerror :: String }

-- | Language chain code is written in
class Language rep where
  -- | type checks. Empty list assumes success
  typeCheck :: rep -> [Error]

  -- | Creates a hash of the given tree
  hash :: rep -> Hash

newtype Hash = Hash { unhash :: Int }
newtype Time = Time { untime :: Int }
newtype Signature = Signature { unsig :: Int }

class ProofType pt where
  mineBlock :: pt -> Hash -> Time -> Signature

--data ChainState =   

-- | index of chain
--   Each chain has a unique index to differentiate it on the network
--   Root chain (index 0) will decide on indexes for other chains
newtype ChainKey = ChainKey { unChainKey :: Int }

-- | Chain represents a block chain within the network. Each chain has their own language
--   and proof type (ie. PoW, PoS, etc.)
data Chain lang proofType =
  Chain {
  index :: Integer 
  }

  
  
