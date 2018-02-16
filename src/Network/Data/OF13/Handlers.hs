{-# LANGUAGE LambdaCase #-}

-- | Common handlers.

module Network.Data.OF13.Handlers where

import           Network.Data.OF13.Server
import           Network.Data.OpenFlow.Messages
import           Network.Data.OpenFlow.MessagesBinary ()
import           Network.Data.OpenFlow.Switch

type Handler msg = Switch -> msg -> IO ()

sendToSwitch :: Switch -> CSMessage -> IO ()
sendToSwitch sw msg = sendMessage sw [msg]

handleHandshake :: Handler SCMessage -> Handler SCMessage
handleHandshake restHandler sw = \case
    SCHello xid -> do
        sendToSwitch sw $ FeaturesRequest xid
    SCEchoRequest xid _ -> do
        sendToSwitch sw $ CSEchoRequest xid []
        sendToSwitch sw $ CSEchoReply xid []
    SCEchoReply _ _ -> return ()
    Features _ features -> do
        putStrLn $ "Handshake with switch #" ++ show (switchID features)
                ++ " completed"

    other -> restHandler sw other

