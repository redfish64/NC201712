module WhiteBoard.Monitor where

--simple module implementing java style monitors in haskell

import Control.Concurrent.MVar
import Data.IORef
import Control.Concurrent
import Control.Monad

data Monitor = Monitor
    { monitorLock :: MVar ThreadId
    , monitorCond :: MVar [MVar ()]
    }

-- | Repeatedly tests @b@ and runs @body@ if false.
whileM :: IO Bool -> IO () -> IO ()
whileM cond body = doit
  where
    doit =
      do
        v <- cond
        if v then
          do body; doit
          else return ()
          

-- | Repeatedly tests @b@ and runs @body@ if false.
-- Collesces results of body into list (result will be in reverse order)
whileMToList :: Monad m => m Bool -> m a -> m [a]
whileMToList cond body = doit []
  where
    doit l =
      do
        v <- cond
        if v then
          do
            i <- body
            doit $ i : l
          else return l
          

-- | Create a new monitor object, which contains the lock as
-- well as the queue of condition variables threads are waiting on.
newMonitor :: IO Monitor
newMonitor =
  do
    ml <- newEmptyMVar
    mc <- newMVar []
    
    return Monitor { monitorLock = ml, monitorCond = mc }

-- | Runs a computation within a monitor.
synchronized :: Monitor -> IO a -> IO a
synchronized m innerProc =
  do
    --first check if there is a monitor look in our name. If so, we can ignore
    --it and run doit anyway. (This is how we do recursive sychronization)
    ownIt <- ownCurrentLock m
    if ownIt then
      do
        --putStrLn "recursive"
        innerProc
      else
      do
        --create a monitor lock, so no one else can run, and then run our
        --IO
        acquireLock m
        v <- innerProc
        releaseLock m
        return v


acquireLock :: Monitor -> IO ()
acquireLock m = myThreadId >>= putMVar (monitorLock m)

releaseLock :: Monitor -> IO ()
releaseLock m = takeMVar (monitorLock m) >> return ()


ownCurrentLock :: Monitor -> IO Bool
ownCurrentLock m =
  do
    myId <- myThreadId
    mt <- tryReadMVar $ monitorLock m
    case mt of
      Just loId -> return $ myId == loId
      _ -> return False

    
    
lockOwner :: Monitor -> IO (Maybe ThreadId)
lockOwner m = tryReadMVar $ monitorLock m

    
    
-- | Inside a 'synchronized' block, releases the lock and waits
-- to be notified
-- Must only be used within a synchronization lock, or an error will be thrown
wait :: Monitor -> IO ()
wait m =
  do
    ownLock <- ownCurrentLock m
    if not ownLock then
      do
        lo <- lockOwner m
        error ("Can't wait, we don't own the lock! Owned by "++show lo) else
      do
    
        --create a new mvar for us to wait on
        myC <- newEmptyMVar

        --add that to the monitorCond list. Since we take the monitorCond first
        --we are guaranteed that no other thread can edit us (because they also take it)
        mc <- takeMVar $ monitorCond m
        putMVar (monitorCond m) $ myC : mc

        --release the synchronization lock (waits can only be within sychronized blocks)
        releaseLock m

        --take our condition, which we set as empty. When a notify occurs, it
        --will put, which will wake us up so we can continue our thread
        takeMVar myC

        --now that we are done waiting, take the synchronization lock again to continue
        acquireLock m

-- | Notifies the monitor that some conditions may have become true,
-- and wakes up one process.
-- Returns false if no thread woke up in response to notification
notify :: Monitor -> IO Bool
notify m =
  do
    mc <- takeMVar $ monitorCond m
    --putStrLn $ "notify: mc length is "++(show $ length mc)
    case mc of
      [] -> do
        putMVar (monitorCond m) mc -- no threads waiting, so just put the monitorCond back
        return False
      myC : mc ->  -- there are thread(s) waiting, so wake one up
        do
          putMVar myC () --wake up the waiting thread
          putMVar (monitorCond m) mc --take the waiting thread off the queue
          return True
   

-- | Notifies all threads waiting on this monitor
notifyAll :: Monitor -> IO ()
notifyAll m =
  do
    mc <- takeMVar $ monitorCond m
    mapM (\myC -> putMVar myC ()) mc
    return ()

---------------------------------------------------------------------
-- Example code:

data Account = Account {
    withdraw :: Int -> IO (),
    deposit :: Int -> IO ()
}

newAccount :: IO Account
newAccount = do
    m <- newMonitor
    balance <- newIORef 0
    return Account
            { withdraw = \n -> synchronized m $ do
                putStrLn ("Withdrawing " ++ show n)
                whileM (fmap (< n) $ readIORef balance) $ wait m
                curr <- readIORef balance
                writeIORef balance (curr - n)
                putStrLn "Withdrawal approved"
            , deposit = \n -> synchronized m $ do
                putStrLn ("Depositing " ++ show n)
                curr <- readIORef balance
                writeIORef balance (curr + n)
                notify m
                return ()
            }

makeAccountWithPendingWithdrawal = do
    a <- newAccount
    makePendingWithdrawal a

makePendingWithdrawal a = do
    forkIO $ do
        withdraw a 20
    return a

_test =
  do
    a <- makeAccountWithPendingWithdrawal
    makePendingWithdrawal a
    makePendingWithdrawal a
    makePendingWithdrawal a
    makePendingWithdrawal a
    makePendingWithdrawal a
    deposit a 1
    deposit a 2
    deposit a 3
    deposit a 20
    deposit a 20
    deposit a 20
    deposit a 20
    deposit a 20
    deposit a 20

_testRecursiveSync =
  do
    m <- newMonitor
    synchronized m $ synchronized m $ putStrLn "hi there"

