-- | Simple controller which reacts on packet-in events.

module Main
    ( main
    ) where

import           Formatting                 (formatToString, shown, (%))

import           Network.Data.OF13.Handlers
import           Network.Data.OF13.Server
import qualified Network.Data.OpenFlow      as OF

main :: IO ()
main = runServer 6633 factory
  where factory sw = handshake sw >> return (messageHandler sw)

handshake :: Switch -> IO ()
handshake sw = sendToSwitch sw $ OF.CSHello 0

messageHandler :: Switch -> Maybe OF.SCMessage -> IO ()
messageHandler sw = \case
    Nothing -> putStrLn "Disconnecting\n"
    Just msg -> handleMsg msg
  where
    handleMsg = \case
        OF.PacketIn xid (OF.PacketInfo (Just bufferId) _ inPort reason _ _) -> do
            putStrLn $ formatToString ("Incoming packet on port "%shown
                                      %" (reason: "%shown%")")
                       inPort reason

            let actions = OF.flood
            let out = OF.bufferedPacketOut bufferId (Just inPort) actions
            sendToSwitch sw $ OF.PacketOut xid out

        OF.PacketIn _ (OF.PacketInfo Nothing _ _ _ _ _) -> do
            putStrLn "Packet in without buffer specified, skipping"

        other -> handleHandshake onUnhandled sw other

    onUnhandled _ msg = putStrLn $ "Unhandled message from switch: " ++ show msg

