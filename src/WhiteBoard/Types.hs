{-# LANGUAGE OverloadedStrings #-}
module WhiteBoard.Types where

import Data.Text as T
import Data.Map.Strict
import Data.Typeable
import Data.TCache
import Data.TCache.DefaultPersistence
import qualified Data.ByteString.Lazy.Char8 as BL(pack,unpack,ByteString(..))
import GHC.Read(readPrec,expectP)
import Text.Read.Lex(Lexeme(..))
import Text.ParserCombinators.ReadPrec(step)
import Data.Sequence(Seq)
import Data.ByteString
import Control.Monad.State
import Control.Monad.Reader
import WhiteBoard.Monitor
import Control.Concurrent.Chan.Unagi
import Control.Concurrent.MVar
import Data.IORef
import Control.Monad.Trans.Maybe
import Data.SortedList as SL

-- | static data for WhiteBoard.  This includes configuration, as well
--   as other static objects, like references to channels.
data WBConf k o = WBConf {
  numWorkingThreads :: Int, -- ^ number of threads to spin up for processing  
  timeBetweenCommitsSecs :: Int,  -- ^ time between intermediary commits to disk
  -- (to save work in case of power failure)
  actionFunc :: WBIMonad k o (), -- ^ function that performs an
  -- appropriate task(s) for a WBObj (which would typically load and
  -- store other objects with other keys)
  inChan :: InChan k, -- ^ Write end of FIFO queue of dirty objects. Filled by the scheduler
    --thread and read by the worker threads
  outChan :: OutChan k, -- ^ Read end of FIFO queue of dirty objects. Populated by the
    --worker threads and read by the scheduler thread
  schedulerChannel :: MVar (SchedulerEvent k) -- ^ Scheduler thread reads from this to handle
    --various events. It uses this to decide when to stop working and save to the cache
    --This is how the worker threads communicate to the scheduler thread.
    -- It's an MVar because the events should happen rarely, and the scheduler shouldn't
    -- be very busy, so it should happen fast, and therefore is ok to block
  }

-- | events messages run by the scheduler
data SchedulerEvent k =
  SEWorkerThreadProcessedItem -- ^ anytime a worker thread processes an item, it
  --sends this event. This is necessary because the scheduler thread can't know
  --when the worker threads are done by monitoring outChan. In other words, both
  --inChan and outChan could be empty, yet worker threads could still be processing
  --because they just haven't finished their last item of work.
  | SETimerWentOff -- ^ the intermediate state is periodically saved to the database
  --this is accomplished by a timer thread that pings the scheduler thread using
  --this message type
  | AddDirtyItems [k]  -- ^ called by the worker thread whenever it makes a
  --change to an object. Also called when an anchor object (initial object) is added.
  deriving (Show)


--TODO 2 we aren't deleting any old data. We will need to delete data, preferrably
-- old data first

  
-- | this is just an IO monad with the WBConf data 
type WBMonad k o = ReaderT (WBConf k o) IO

{- | this is the monad used within the user-defined actionFunc. It can
     be used to store an object, or load an object associated to a
     key.

     It's addition to WBMonad is a reader that provides the current
     key and object being worked on.
-}
type WBIMonad k o = ReaderT (k,ObjMeta k o) (WBMonad k o)


runWBMonad :: (WBConf k o) -> WBMonad k o x -> IO x
runWBMonad wbc m = runReaderT m wbc

{- | Meta data about the objects within WhiteBoard.

     The objects refered to (aka loaded), and objects stored by the
     user-defined action func while it's working on a object are saved
     to the db using this object.
-}
data ObjMeta k o = ObjMeta {
  key :: k,
  payload :: SortedList o,  -- ^ objects stored to this key.
  refererKeys :: [k], -- ^ objects that load, or store this object, so
                      -- if it changes, they become dirty. In the case
                      -- of storers, if there are multiple stores, we
                      -- have to report an error, so this keeps track
                      -- of them as well.
  referers :: Maybe [ObjMeta k o], -- ^ populated on demand from
                                   -- referersKeys, not saved to db
  storedObjKeys :: [k], -- ^ objects stored by this object. We need
                        -- this to be able to clean up objects, when
                        -- the objects that are stored change when we
                        -- rerun.

  storedObjs :: Maybe [ObjMeta k o] -- ^ populated on demand from
                                    -- storedObjsKeys. not saved to db
  } deriving (Show,Read)

instance (Ord o, Read o) => Read (SortedList o) where
  readsPrec p = (\s -> fmap (\(l,str) -> (toSortedList l, str)) (readsPrec p s))

instance (Show k) => Indexable (ObjMeta k o) where
  key = show . WhiteBoard.Types.key

{- | all objects stored into the whiteboard must implement this. Note that "Ord" is required
     to help keep the system consistent when multiple items are added and read. (Since the
     system is multi threaded, there is no way to determine which object will be written
     first for two objects that share the same key) -}
class (Typeable o,Serializable o,Eq o,Show o,Read o,
       Ord o) => WBObj o

instance (Keyable k, WBObj o) => Serializable (ObjMeta k o) where
  --TODO 3.5 PERF make these more binary'ie
  --TODO 2 make sure we take advantage of the serialization of o
  serialize= BL.pack . show
  deserialize= read . BL.unpack

type WBId = Text

class (Typeable k,Serializable k,Eq k,Show k,Read k) => Keyable k

data DirtyQueue k = DirtyQueue [k] deriving (Read,Show,Typeable)

-- instance Eq (DirtyQueue k) where
--   _ == _ = True -- DirtyQueue is a singleton instance, so there never will be a need to compare items
  
-- instance Ord (DirtyQueue k) where
--   _ == _ = True -- DirtyQueue is a singleton instance, so there never will be a need to compare items
  

-- instance (Keyable k) => WBObj (DirtyQueue k)

instance (Keyable k) => Serializable (DirtyQueue k) where
  --TODO 3.5 PERF make these more binary'ie
  --TODO 2 make sure we take advantage of the serialization of o
  serialize= BL.pack . show
  deserialize= read . BL.unpack

instance Indexable (DirtyQueue k) where
  key _ = "DIRTY QUEUE!"

    
   
