-- | Simple controller which reacts on packet-in events.

module Main
    ( main
    ) where

import           Formatting               (formatToString, shown, (%))

import           Network.Data.OF13.Server
import qualified Network.Data.OpenFlow    as OF

main :: IO ()
main = runServer 6633 factory
  where factory sw = handshake sw >> return (messageHandler sw)

sendToSwitch :: Switch -> OF.CSMessage -> IO ()
sendToSwitch sw msg = sendMessage sw [msg]

handshake :: Switch -> IO ()
handshake sw = sendToSwitch sw $ OF.CSHello 0

messageHandler :: Switch -> Maybe OF.SCMessage -> IO ()
messageHandler sw = \case
    Nothing -> putStrLn "Disconnecting\n"
    Just msg -> handleMsg msg
  where
    handleMsg = \case
        OF.SCHello xid -> do
            sendToSwitch sw $ OF.FeaturesRequest xid
        OF.SCEchoRequest xid _ -> do
            sendToSwitch sw $ OF.CSEchoRequest xid []
            sendToSwitch sw $ OF.CSEchoReply xid []
        OF.SCEchoReply _ _ -> return ()
        OF.Features _ features -> do
            putStrLn $ formatToString ("Handshake with switch #"%shown%" completed")
                       (OF.switchID features)

        OF.PacketIn xid (OF.PacketInfo (Just bufferId) _ inPort reason _ _) -> do
            putStrLn $ formatToString ("Incoming packet on port "%shown
                                      %" (reason: "%shown%")")
                       inPort reason

            let actions = OF.flood
            let out = OF.bufferedPacketOut bufferId (Just inPort) actions
            sendToSwitch sw $ OF.PacketOut xid out
        OF.PacketIn _ (OF.PacketInfo Nothing _ _ _ _ _) -> do
            putStrLn "Packet in without buffer specified, skipping"

        msg -> putStrLn $ "Unhandled message from switch: " ++ show msg

