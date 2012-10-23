{-# LANGUAGE RankNTypes, RecordWildCards, FlexibleInstances #-}

-- Example of using device-check
import Test.DeviceCheck
import Control.Concurrent
import Data.Monoid
import Control.Monad

main = do
     v_in <- newEmptyMVar
     v_out <- newEmptyMVar

     let loop :: Int -> IO ()
         loop n = do
             i <- takeMVar v_in
             putMVar v_out n
             loop (i + n)

     forkIO $ loop 0

     let dut :: DUT Int
         dut = DUT
             { send_in = putMVar v_in
             , recv_out = takeMVar v_out
             }

     runDutM "seed" (callout dut) program

     return ()

------------------------------------------------------------------------------

program :: DutM (ExampleCmd Int) ()
program = do
        property prop
        sender <> recvr
 where
        sender = forever $ do
                        w1 <- randR (0,20)
                        wait w1
                        d2 <- randR (0,255::Int)
                        send (fromIntegral d2)
        recvr = forever $ do
                        w1 <- randR (0,20)
                        wait w1
                        recv

prop :: Prop (ExampleCmd Int)
prop = Prop "prop" $ \ cmds -> step (map send1 cmds) (map recv1 cmds) 0
  where
      step (Nothing:is) (Nothing:os) n = Nothing : step is os n
      step (Nothing:is) (Just (Ret b):os)  n
        | b == n = Nothing : step is os n
        | otherwise = repeat (Just "bad")
      step (Just a:is) (Nothing:os)  n = Nothing : step is os (n + a)
      step (Just a:is) (Just (Ret b):os)   n
        | b == n = Nothing : step is os (n + a)
        | otherwise = repeat (Just "bad")

------------------------------------------------------------------------------

send :: a -> DutM (ExampleCmd a) ()
send d = putCmd0 $ mempty { send1 = Just d }

recv :: DutM (ExampleCmd a) a
recv = putCmd1 $ \ reply -> mempty { recv1 = Just reply }

------------------------------------------------------------------------------

data DUT a = DUT
     { send_in :: a -> IO ()
     , recv_out :: IO a
     }

data ExampleCmd a resp = ExampleCmd
        { send1 :: Maybe a
        , recv1 :: Maybe (resp a)
        }

-- This is called once per cycle.
callout :: (Num a) => DUT a -> ExampleCmd a Reply -> IO (ExampleCmd a Ret)
callout  (DUT { .. }) (ExampleCmd { .. }) = do
--        print "callout"
        send1' <- case send1 of
                   Nothing -> do
                           send_in 0
                           return Nothing
                   Just u -> do
                           send_in u
                           return $ Just u

        recv1' <- case recv1 of
                   Nothing -> do
                           _ <- recv_out
                           return Nothing
                   Just (Reply resp) -> do
                           u <- recv_out
                           resp u
                           return $ Just (Ret u)

        let cmd = ExampleCmd
                { send1 = send1'
                , recv1 = recv1'
                }

--        print cmd
        return cmd



instance Show a => Show (ExampleCmd a Ret) where
        show (ExampleCmd { .. }) =
                "FifoCmd { send1 = " ++ show send1 ++
                        ", recv1 = " ++ show recv1 ++
                        "}"

instance Monoid (ExampleCmd a b) where
        mempty = ExampleCmd
         { send1 = Nothing
         , recv1 = Nothing
         }
        mappend f1 f2 = ExampleCmd
         { send1 = send1 f1 `mplus` send1 f2
         , recv1 = recv1 f1 `mplus` recv1 f2
         }

