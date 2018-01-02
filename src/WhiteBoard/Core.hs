{-# LANGUAGE OverloadedStrings #-}
module WhiteBoard.Core(createWBConf,addAnchorObjects,storeObject,loadObject,startWhiteBoard,finishWhiteBoard) where

{- The core system is a set of threads and a ..... TODO 2 finish this documentation
This system works by having a single scheduler thread that takes care of when
   worker threads should process items, when they should stop, so we can save intermediate
   data, or write finished results after everything has been processed -}

import WhiteBoard.Types as WT
import Data.TCache
import Data.Typeable
import Data.TCache
import Data.TCache.DefaultPersistence
import Data.Sequence as S(Seq,empty, (><),fromList)
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as BL(pack,unpack,ByteString(..))
import qualified Data.ByteString as B(pack,unpack,ByteString(..))
import WhiteBoard.Monitor
import Control.Concurrent.Chan.Unagi 
import Control.Monad.Loops
import Data.IORef
import Control.Concurrent.MVar
import Control.Concurrent (forkIO,threadDelay, ThreadId(..))
import Control.Monad.State
import Data.SortedList as SL
import Control.Monad.Trans.Maybe

--TODO 3 choosable file directory?
-- | Opens a whiteboard. If the whiteboard doesn't exist already, it will be created empty.
createWBConf :: (Keyable k, WBObj o) => (WBIMonad k o ()) -> IO (WBConf k o)
createWBConf actionFunc =
  do
    wIR <- newIORef 0
    (ic,oc) <- newChan
    m <- newEmptyMVar
    ip <- newIORef 0
    ia <- newIORef 0
    
    atomically $
      do
        return $ WBConf { numWorkingThreads = 5, timeBetweenCommitsSecs = 60,
                          actionFunc = actionFunc,
                          inChan = ic, outChan = oc,
                          schedulerChannel = m
                          }



-- | Adds objects that will remain in the system until later deleted
-- using removeAnchorObjects Any other object will only exist as long
-- as another objects action method stores it. So, anchor objects are
-- special, that way.
addAnchorObjects :: (WBObj obj, Keyable k) => [(k,obj)] -> WBMonad k obj ()
addAnchorObjects kv =
  -- TODO 1.5 we need to look at dealing with updating an anchor
  -- object when it already exists (error out, or replace?), or when
  -- it is being refered to by other objects (put them in the dirty
  -- queue)
  do
    wbc <- ask
    updatedKeys <- lift $ atomically $
                   --save off the items to the db
      saveItems kv
    --notify the scheduler to add the items to the dirty queue for processing
    lift $ addDirtyItems wbc updatedKeys
  where
    --saves a list of items, returning the keys of the objects that are dirty (ie weren't
    --already there, or were there but had a differing value)
    saveItems :: (WBObj obj, Keyable k) => [(k,obj)] -> STM [k]
    saveItems items = foldM
      (\l i -> 
         do
           mom <- saveItem i
           case mom of
             Nothing -> return l
             Just om -> return $ (WT.key om) : l
      ) [] items
    --saves an individual item. Returns a value if dirty
    saveItem :: (WBObj obj, Keyable k) => (k, obj) -> STM (Maybe (ObjMeta k obj))
    saveItem (k, v) =
      do
        --TODO PERF 3 we may want to use a special method here so we get an abbr
        -- (faster) value for the key
        --TODO PERF 4 we may also want to consider modifying TCache to accept binary
        --keys rather than just strings
        let ref = getDBRef (show k) -- :: DBRef (ObjMeta kt))
        let p = (SL.singleton v)
        
        --look for an existing object
        mmo <- readDBRef ref -- :: STM (Maybe (ObjMeta kt)))
        case mmo of
          --object doesn't exist so create a new one and save it
          Nothing -> 
            fmap Just (writeObj ref (ObjMeta {WT.key=k, payload=p,
                                                       refererKeys = [],
                                                       referers = Nothing,
                                                       storedObjKeys = [],
                                                       storedObjs = Nothing}))
                                             
          Just om@(ObjMeta { payload=existingPayload }) -> 
            if existingPayload == p
            then return Nothing  -- object hasn't changed, so not dirty
            else
              fmap Just $ writeObj ref (updateObjMetaForPayload om p)  --object has changed, so update and save it

    writeObj :: (WBObj o, Keyable k) => DBRef (ObjMeta k o) -> (ObjMeta k o) -> STM (ObjMeta k o)
    writeObj ref om =
      do
        writeDBRef ref om
        return om
        

    updateObjMetaForPayload :: ObjMeta k obj -> SortedList obj -> ObjMeta k obj
    updateObjMetaForPayload om p = om { payload = p }


readDBRef' :: (Indexable x, Typeable x, Serializable x) => DBRef x -> STM x
readDBRef' dbr =
  do
    (Just v) <- readDBRef dbr
    return v
    

--TODO PERF 4 we may want to dump STM. There is no real need for it, because if we get
--a wrong answer, it doesn't matter, because we'd be given the correct elements and marked
--dirty later
  
-- Tells the scheduler thread there are dirty items. The scheduler thread will either add
-- them to the dirty queue 
addDirtyItems :: (Keyable k, WBObj o) => WBConf k o -> [k] -> IO ()
addDirtyItems wbc items =
  do
    putMVar (schedulerChannel wbc) $ AddDirtyItems items
    
    -- mapM newDBRef items
    -- addToDirtyQueue wbd items
    -- return ()


-- addToDirtyQueue :: (Serializable k, Eq k, Typeable k) => WhiteBoardData k -> [ObjMeta k] -> IO ()
-- modifyObject :: ((Maybe x) -> x) -> WBMonad ()
-- modifyObject = undefined


getExistingOrEmptyObject :: (Keyable k, WBObj o) => k -> STM (ObjMeta k o)
getExistingOrEmptyObject k =
  do
    --get the existing object.. if it doesn't exist, create an empty one
    let eor = getDBRef (show k)
    meo <- readDBRef eor
    return $ maybe (ObjMeta {
                               WT.key = k,
                               payload = toSortedList [],
                               refererKeys = [],
                               referers = Just [],
                               storedObjKeys = [],
                               storedObjs = Just []
                               }
                  ) 
              id meo

addToReferers :: (Keyable k, WBObj o) => k -> ObjMeta k o -> ObjMeta k o -> ObjMeta k o
addToReferers key obj om =
        om {
            refererKeys = key : (refererKeys om),
            referers = fmap (obj :) (referers om) 
            } -- (the happy line)

  

-- | used by keyToAction in WBConf to store an object. Key and object of stored object
--   is passed in and the result of the store is returned
storeObject :: (Keyable k, WBObj o) => k -> o -> WBIMonad k o (SortedList o)
storeObject k o =
  do
    (storerKey, storerObj) <- ask

    (shouldAddDirtyItems, eo) <- liftIO $ atomically $ do
      eo <- getExistingOrEmptyObject k
      --update object for this store action
      let no = eo { payload = insert o (payload eo) }

      -- add storer as a referer. We do this because if there is some
      -- error such as multiple storers, each storer must be returned
      -- an error. So, whenever a new storer is added or one taken
      -- away, the other storers need to be rerun
      let no' = addToReferers storerKey storerObj no 

      return  ((payload no') == (payload eo), eo)

    if shouldAddDirtyItems then
      do
        wbc <- lift $ ask
        liftIO $ addDirtyItems wbc (refererKeys eo) -- note we use 'eo'(existing object) here
         -- because we don't want to rerun the current storer
      else return ()

    return (payload eo)
      

data LoadResult o = LRSuccess o | LRErrorEmpty | LRErrorMultStorers

loadObject :: (Keyable k, WBObj o) => k -> WBIMonad k o (SortedList o)
loadObject k =
  do
    (loaderKey, loaderObj) <- ask
    
    liftIO $ atomically $ do
      --get the existing object.. if it doesn't exist, create an empty one
      eo <- fmap (addToReferers loaderKey loaderObj) $ getExistingOrEmptyObject k

      return (payload eo)

-- TODO PERF 3 we need to think about cache and how to deal with removal
-- the below "defaultCheck" will clear items from the cache once they exceed cache size
-- it will clean all elements that haven't been referenced in half the frequency
--TODO 2 make frequency and cacheSize parameters (or in WBConf)
-- TODO 2.6 verify that TCache is behaving how we want when it synchronizes to disk
-- TODO 2.3 use different persist mechanism so that we can handle billions of entries
-- | starts the whiteboard background threads
startWhiteBoard :: (Keyable k, WBObj o) => WBConf k o -> IO ()
startWhiteBoard wbc = do
  forkIO $ runReaderT schedulerThread wbc
  replicateM (numWorkingThreads wbc) (forkIO $ (runReaderT workerThread wbc))
  return ()
             
-- | saves the cache to the database
finishWhiteBoard :: WBConf k o -> IO ()
finishWhiteBoard _ = return ()
  



data STReadEventMode = STWorkingMode | STStoreToCacheMode


--TODO 2.9 look into using something like debrun indexes to merge
--nodes together that look the same, but use slightly different
--variables. We'd have to have some way of extracting symbol names out
-- of symbols to do this.
--TODO 2.9 Think about whether to keep no longer used items in case
-- the calculation might be needed again. We would attach a hash
-- to each item, so that if it is reused again later, we can simply
-- undelete it rather than recalculate it from scratch

{- |

   The scheduler thread is the heart of the system. Any items that
   are added to the queue to process is done by communicating to
   the scheduler thread.

   It also receives a notification whenever a worker thread completes
   work on a dirty item.

   The scheduler thread operates in the following states and state
   transitions:

     1. Working - worker threads process dirty items. All dirty items
     created by working threads or outside of the system (through
     anchor objects) go back into their input queue.

     ** transistion - when save timer goes off, or all threads stop
        working, we transition

     2. Preparing to save - All dirty items produced from anchor
     objects or worker threads is switched to an alternative list for
     saving to the db. Worker Thread input queue is drained into list
     for db.

     ** transistion - when all threads indicate they are paused, we
        move to stage 3

     3. Save - save the db list and all cached, modified items. Dump
     db list of dirty items back into processing queue for transition
     to stage 1. If there are no dirty items to process, wait for an
     item to come in before going back to stage 1 (so we don't busy
     loop endlessly)

   Adding of dirty items, the timer going off, and thread pauses are
   all handled by a single mvar which client threads write to and the
   scheduler thread reads. This way, there can be no race conditions,
   because if the sceduler thread is handling one event, it can't
   receive another while its working that would confuse it.
 -}
schedulerThread :: (Keyable k, WBObj o) => WBMonad k o ()
schedulerThread =
  do
    --start in mode 1 prep 
    runStateT mode1Prep (SchedulerData 0 0 [])
    return ()
  where
    readEvent :: WBMonad k o (SchedulerEvent k)
    readEvent = do
      wbc <- ask
      liftIO $ takeMVar (schedulerChannel wbc)

    mode1Prep :: (Keyable k) => StateT (SchedulerData k) (WBMonad k o) ()
    mode1Prep =
      do
        liftIO $ putStrLn "mode1Prep"
        lift $ wakeUpOnTimer
        mode1

    --working mode
    mode1 :: (Keyable k) => StateT (SchedulerData k) (WBMonad k o) ()
    mode1 =
      do
        liftIO $ putStrLn "mode1"
        wbc <- ask

        --read the next event
        event <- lift $ readEvent
        liftIO $ putStrLn $ "readEvent " ++ (show event)
        case (event) of
          SEWorkerThreadProcessedItem ->
            do
              --TODO 2.5 in mode1, we do this, so we don't hang around doing
              --nothing when all the work is done.. We may want to
              --change this so that if at least 5 seconds hasn't gone
              --by, we don't go to mode 2. That way we can still be reponsive
              --if we get a lot of short bursts of simple work
              workerThreadProcessedItem mode1 mode2Prep
          SETimerWentOff -> mode2Prep --when the timer goes off, we
                                      --need to do a save of our
                                      --partially completed work and
                                      --then continue, so we go to mode2
          AddDirtyItems ks -> do
            liftIO $ putStrLn $ "dirty items " ++ show (length ks)
            --write to the input queue so the workers can get started
            addDirtyItems' ks
            mode1

    --updates SchedulerData to reflect worker thread processing an item.
    --if all threads have stopped, runs nextMode, otherwise stays in currMode
    workerThreadProcessedItem :: (Keyable k) => StateT (SchedulerData k) (WBMonad k o) ()
      -> StateT (SchedulerData k) (WBMonad k o) ()
      -> StateT (SchedulerData k) (WBMonad k o) ()
    workerThreadProcessedItem currMode nextMode =
      do
        modify (\ss -> ss { itemsProcessed = succ (itemsProcessed ss) })
        ps <- fmap isProcessingStopped get 
        if ps then
          --no threads are processing and the queue is empty, work is done
          --go to mode 2 to save.
          --
          nextMode
          else
          currMode

    -- returns true if number of items added to the queue equals the number of items processed
    -- this means the queue should be empty and all the threads stopped
    isProcessingStopped :: (SchedulerData k) -> Bool
    isProcessingStopped ss = (itemsAdded ss) - (itemsProcessed ss) == 0

    -- adds dirty items to queue and increments SchedulerData.itemsAdded by number of items
    addDirtyItems' :: (Keyable k) => [k] -> StateT (SchedulerData k) (WBMonad k o) ()
    addDirtyItems' ks =
      do
        modify (\ss -> ss { itemsAdded = (itemsAdded ss) + (length ks) })
        wbc <- lift $ ask
        --writes the items to the channel read by the worker threads
        liftIO $ writeList2Chan (inChan wbc) ks
            
    --prepare to save to cache
    mode2Prep :: (Keyable k) => StateT (SchedulerData k) (WBMonad k o) ()
    mode2Prep =
      do
        liftIO $ putStrLn "mode2Prep"
        wbc <- ask

        --TODO 3 empty queue of dirty items so threads stop quicker
        -- dirtyItems <- whileMToList (fmap not (liftIO $ isDirtyQueueEmpty wbc)) (liftIO $ readChan (outChan wbc))
        -- threadsWaiting <- get
        
        mode2

    --as noted above, here we wind down the worker threads if they have current work to do
    --then go to mode 3 to save
    mode2 :: (Keyable k) => StateT (SchedulerData k) (WBMonad k o) ()
    mode2 =
      do 
        liftIO $ putStrLn "mode2"

        --check if there is still more work to do
        ps <- fmap isProcessingStopped get
        if ps then mode3
          else
          do
            --wait for all work to be finished
            wbc <- ask
            event <- lift $ readEvent
            
            case (event) of
              SEWorkerThreadProcessedItem ->
                do
                  workerThreadProcessedItem mode2 mode3
              SETimerWentOff -> mode2 --ignore any timer event
              AddDirtyItems ks -> --we can still get anchor items added at any time
                --so we put them in frozen state
                do
                  modify (\ss -> ss { frozenKeys = (ks ++ (frozenKeys ss)) } )
                  mode2
              
    --TODO 3 consider that in AN, we'll may have the need to commit some data
    --directly to the disk as soon as it's received for less volatility... not sure
    --about this. 
    mode3 :: (Keyable k) => StateT (SchedulerData k) (WBMonad k o) ()
    mode3 = do
      liftIO $ putStrLn "mode3"
      --save the entire cache as a single transaction (so that if there is a power failure,
      --we either commit all or none)

      frozenKeys' <- fmap frozenKeys get
      liftIO $ atomically $ do
        --we need to save off the new set of dirty keys
        
        --note that any previous set of dirty keys is discarded.
        --This is because when we start up, we copy all the dirty keys into
        --the processing queue, and so they will all be cleared anyway.
        ref <- newDBRef $ DirtyQueue []
        writeDBRef ref $ DirtyQueue frozenKeys'

      wbc <- ask
      
      --now that were done with the save, we get back to work
      if (isEmpty frozenKeys') then
        mode3Purgatory --there are no dirty keys to process, so we wait until one comes in
        else
        do
          --write out the list to the active queue so the threads start processing
          addDirtyItems' frozenKeys'
          mode1Prep
          

    --here, we wait until we get an item to process
    mode3Purgatory :: (Keyable k) => StateT (SchedulerData k) (WBMonad k o) ()
    mode3Purgatory = do
      liftIO $ putStrLn "mode3Purgatory"
      event <- lift $ readEvent

      case (event) of
        AddDirtyItems ks -> do
          --write to the input queue so the workers can get started
          addDirtyItems' ks
          
          mode1Prep --go back to processing
        _ -> mode3Purgatory


    --fork a thread to notify ourselves periodically to wake up when
    --we need to save the intermediate state
    wakeUpOnTimer :: (Keyable k) => WBMonad k o ()
    wakeUpOnTimer =
      do
        wbc <- ask
        lift $ startTimerThread (timeBetweenCommitsSecs wbc * 1000 * 1000) $ 
          putMVar (schedulerChannel wbc) SETimerWentOff
        return ()


data SchedulerData k = SchedulerData {
  itemsProcessed :: Int, --total items processed since start of scheduler thread
  itemsAdded :: Int,  -- total items added to queue since start of scheduler thread
  frozenKeys :: [k] --these are items added while we are saving
                 --(if they exist, these will only be anchor objects, since all
                 --worker threads will be stopped at that time). We
                 --hold on to them until we're ready to work again
              
  }

--TODO 3: would like to replace all the "StateT ..." references with a type, but it requires rankntypes...not sure what this is
--type SchedulerMonad k o = (Keyable k, WBObj o) => StateT (SchedulerData k) (WBMonad k o) ()

-- | returns true if the dirty queue is empty
isDirtyQueueEmpty :: SchedulerData k -> Bool
isDirtyQueueEmpty c = itemsProcessed c - itemsAdded c == 0

isEmpty :: [x] -> Bool
isEmpty [] = True
isEmpty _ = False


--starts a single event timer thread
startTimerThread :: Int -> IO () -> IO ThreadId
startTimerThread waitTimeMicroSecs actionToPerform =
  forkIO $ (threadDelay waitTimeMicroSecs >> actionToPerform)

-- | thread for running tasks in the dirty queue
workerThread :: (Keyable k, WBObj o) => WBMonad k o ()
workerThread =
  do
    forever processItem
  where
    processItem :: (Keyable k, WBObj o) => WBMonad k o ()
    processItem =
      do
        wbc <- ask

        --read the key for the next dirty item
        key <- liftIO $ readChan (outChan wbc)

        --get the object for the key
        (Just obj) <- (lift $ atomically $ readDBRef $ getDBRef (show key)) -- :: (Keyable k, WBObj o) => WBMonad k o (Maybe (ObjMeta k o))

        --use the key to create a task, and then run it
        runReaderT (actionFunc wbc) (key,obj)

        --we did a little bit of work, so we update the stats
        liftIO $ putMVar (schedulerChannel wbc) $ SEWorkerThreadProcessedItem

        return ()

-- -- | returns the WBObj that is being processed on behalf of the
-- --   thread
-- getCurrWBObj :: WBIMonad k o (WBObj k o)
-- getCurrWBObj =
--   do
--     (_,o) <- ask
--     let (Payload
