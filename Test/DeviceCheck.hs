{-# LANGUAGE ScopedTypeVariables, BangPatterns, RecursiveDo, UndecidableInstances,
             FlexibleContexts, KindSignatures, RankNTypes, GADTs, FlexibleInstances #-}
module Test.DeviceCheck
        ( DutM
        , Reply(..)
        , Ret(..)
        , Prop(..)
        , runDutM
        , wait
        , rand
        , randR
        , property
        , putCmd0
        , putCmd1
        , putCmd2
        , approxLemma
        ) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad
import System.IO
import Data.Monoid
import Data.Maybe

import Control.Concurrent

import Control.Concurrent.MVar
import System.IO.Unsafe
import Control.Monad.Trans.Class
import System.Random
import Control.Monad

-- The monad

data DutM :: ((* -> *) -> *) -> * -> * where
        DutM :: (TestEnv f -> IO a) -> DutM f a

instance Monad (DutM f) where
        return a = DutM $ \ env -> return a
        (DutM m) >>= k = DutM $ \ env -> do
                r <- m env
                case k r of
                  DutM m -> m env

instance MonadIO (DutM f) where
        liftIO m = DutM $ \ _ -> m

data TestEnv f = TestEnv
        { env_rand  :: forall r . (Random r) => IO r -- a random number generator
        , env_randR :: forall r . (Random r) => (r,r) -> IO r
        , the_cmds  :: MVar (StepCmd f)  -- where to send the commands
        }

data StepCmd f
        = StepDone               -- 1 cycle, no more commands
        | StepCmd (f Reply)      -- 1 cycle
        | RegProp (Prop f)       -- no cycles

data Reply a = Reply (a -> IO ())

data Ret  a = Ret a
        deriving Show

-- | A prop is named test that should always be true for a specific test.
data Prop f = Prop String ([f Ret] -> [Maybe String])

-----------------------------------------------------------------

runDutM :: String -> (f Reply -> IO (f Ret)) -> DutM f () -> IO ()
runDutM seed action prog = do
        cmd_var <- newEmptyMVar
        let std0 :: StdGen = read seed
        var <- newMVar std0
        env <- return $ TestEnv
          { env_rand = do
                std <- takeMVar var
                let (n,std') = random std
                putMVar var std'
                return n
          , env_randR = \ (a,b) -> do
                std <- takeMVar var
                let (n,std') = randomR (a,b) std
                putMVar var std'
                return n
          , the_cmds = cmd_var
          }

        forkIO $ case prog of
           DutM m -> do m env

        dut_interp action cmd_var

--        if n `mod`  == 0 then do { putChar '.' ; hFlush stdout } else return ()

dut_interp :: (f Reply -> IO (f Ret)) -> MVar (StepCmd f) -> IO ()
dut_interp callout cmd_var = loop 0 []
  where
     loop n checked = do
        if n `mod` 10000 == 0 then do { putChar '.' ; hFlush stdout } else return ()
        -- get step commands
        (props,opt_cmd) <- takeStepCmd cmd_var
        case opt_cmd of
          Nothing -> return ()
                         -- do step
          Just cmd -> do ret <- callout cmd
                         props' <- sequence [ mkProp prop | prop <- props ]
                         loop2 n ret (props' ++ checked)

     loop2 n ret checked = do
            answers <- sequence [ f ret | f <- checked ]
            case catMaybes answers of
              []      -> loop (n+1) checked
              (txt:_) -> do print ("failed at #",n,txt)
                            return ()

mkProp :: Prop f -> IO (f Ret -> IO (Maybe String))
mkProp (Prop nm f) = do
        in_var <- newEmptyMVar
        out_var <- newEmptyMVar
        forkIO $ do
                let in_loop = do
                        state <- takeMVar in_var
                        states <- unsafeInterleaveIO in_loop
                        return $ state : states
                    -- You need the ! pattern here to make sure
                    -- the unsafeInterleaveIO is consumed by
                    -- *this* thread.
                    loop ((!o):os) = do
--                        print o
                        putMVar out_var o
                        loop os
                    loop [] = return ()
                ins <- in_loop
                loop (f ins)

        return $ \ state -> do
                putMVar in_var state
                takeMVar out_var

---------------------------------------------------------
-- Monad Commands

putCmd0 :: Monoid (f Reply) => f Reply -> DutM f ()
putCmd0 cmd = DutM $ \ env -> do
        putMVar (the_cmds env) (StepCmd $ cmd)

putCmd1 :: Monoid (f Reply) => (Reply a -> f Reply) -> DutM f a
putCmd1 cmd = DutM $ \ env -> do
        v <- newEmptyMVar
        -- You need to use the reply argument (otherwise the takeMVar v hangs)
        putMVar (the_cmds env) (StepCmd $ cmd (Reply $ putMVar v))
        takeMVar v

putCmd2 :: Monoid (f Reply) => (Reply a -> Reply b -> f Reply) -> DutM f (a,b)
putCmd2 cmd = DutM $ \ env -> do
        a <- newEmptyMVar
        b <- newEmptyMVar
        -- You need to use the reply argument (otherwise the takeMVar v hangs)
        putMVar (the_cmds env) (StepCmd $ cmd (Reply $ putMVar a) (Reply $ putMVar b))
        a' <- takeMVar a
        b' <- takeMVar b
        return (a',b')

wait :: Monoid (f Reply) => Int -> DutM f ()
wait 0 = return ()
wait n = do
        putCmd0 mempty
        wait (n - 1)

rand :: (Random r) => DutM f r
rand = DutM $ \ env -> env_rand env

randR :: (Random r) => (r,r) -> DutM f r
randR (a,b) = DutM $ \ env -> env_randR env (a,b)

property :: Prop f -> DutM f ()
property prop = DutM $ \ env -> do
        putMVar (the_cmds env) (RegProp prop)

instance Monoid (f Reply) => Monoid (DutM f ()) where
        mempty      = return ()
        mappend a b = mconcat [a,b]
        mconcat     = parDutM

-----------------------------------------------------------------

-- | Approx. lemma build a O(n^2) prop, that can be used for establishing small examples.
approxLemma :: String -> ([f Ret] -> Maybe String) -> Prop f
approxLemma str f = Prop str $ \ xs -> [ f (take n xs) | n <- [0..] ]

-----------------------------------------------------------------
-- Internals

parDutM :: (Monoid (f Reply)) => [ DutM f () ] -> DutM f ()
parDutM ms = DutM $ \ env -> do
        vs <- sequence
                  [ do v <- newEmptyMVar
                       forkIO $ do f (env { the_cmds = v })
                                   forever $ putMVar v StepDone
                       return v
                  | DutM f <- ms
                  ]

        -- returns when all the commands
        let loop = do
                (regs,vals) <- liftM unzip $ sequence [ takeStepCmd v | v <- vs ]
                case (concat regs,mconcat vals) of
                  ([],Nothing) -> return ()
                  (props,opt_cmd) -> do
                          sequence_ [ putMVar (the_cmds env) (RegProp prop)
                                    | prop <- props
                                    ]
                          putMVar (the_cmds env) $ case opt_cmd of
                                        Nothing -> StepDone
                                        Just cmd -> StepCmd cmd
                          loop

        -- call loop until all the sub-threads are done
        loop



takeStepCmd :: MVar (StepCmd f) -> IO ([Prop f],Maybe (f Reply))
takeStepCmd var = loop []
  where
        loop regs = do
--                print "take step"
                v <- takeMVar var
--                print "taken step"
                case v of
                   StepDone -> return (regs,Nothing)
                   StepCmd cmd -> return (regs,Just cmd)
                   RegProp prop -> loop (regs ++ [prop])

