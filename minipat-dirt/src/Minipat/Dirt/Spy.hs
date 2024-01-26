{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Spy where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Dahdit.Midi.Osc (Datum (..), Msg (..), Packet (..))
import Dahdit.Network (Conn (..), HostPort (..), resolveAddr, runDecoder, runEncoder, udpServerConn)
import Data.Acquire (allocateAcquire)
import Data.Sequence qualified as Seq
import Network.Socket qualified as NS

oscSpyLocal :: Int -> Int -> IO ()
oscSpyLocal spyPort destPort = oscSpy (mkHp spyPort) (mkHp destPort)
 where
  mkHp = HostPort (Just "127.0.0.1")

oscSpy :: HostPort -> HostPort -> IO ()
oscSpy spyHost destHost = runResourceT $ do
  destAddr <- liftIO (resolveAddr destHost)
  (_, srvConn) <- allocateAcquire (udpServerConn Nothing spyHost)
  liftIO (oscSpyLoop destAddr srvConn)

-- Just testing whether tidal connects to this or not
xformMsg :: Packet -> Packet
xformMsg = \case
  PacketMsg (Msg addr ds)
    | addr == "/dirt/handshake/reply" ->
        let ds' = Seq.update 3 (DatumInt32 57111) ds
        in  PacketMsg (Msg addr ds')
  p -> p

oscSpyLoop :: NS.SockAddr -> Conn NS.SockAddr -> IO ()
oscSpyLoop destAddr (Conn dec enc) = go Nothing
 where
  go maySrcAddr = do
    (recvAddr, res) <- runDecoder dec
    case res of
      Left err -> print err
      Right (msg :: Packet) -> do
        let msg' = xformMsg msg
        print (recvAddr, msg')
        if recvAddr == destAddr
          then do
            case maySrcAddr of
              Just srcAddr -> runEncoder enc srcAddr msg'
              Nothing -> pure ()
            go maySrcAddr
          else do
            runEncoder enc destAddr msg'
            go (Just recvAddr)
