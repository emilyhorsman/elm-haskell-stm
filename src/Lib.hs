{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( highScoreServer
    ) where

import           Control.Concurrent             (forkIO)
import           Control.Concurrent.Async       (race)
import           Control.Concurrent.STM         (TChan, atomically, readTChan,
                                                 writeTChan)
import           Control.Monad                  (forever)
import           Control.Monad.IO.Class         (liftIO)
import           Data.Text                      (Text)
import           Data.Text.IO                   as Tio
import           Network.HTTP.Types             (status400)
import           Network.Wai                    (Application, responseLBS)
import           Network.Wai.Handler.Warp       (run)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets             as WS

import qualified HighScores


wsApp :: TChan HighScores.CentralMessage -> WS.ServerApp
wsApp centralMessageChan pendingConn = do
    -- This function handles each new Connection.
    conn <- WS.acceptRequest pendingConn
    WS.forkPingThread conn 1

    -- Get a new message channel for this client.
    clientMessageChan <- atomically HighScores.newClientMessageChan

    -- Inform the central message channel that we have a newly connected user.
    atomically $ writeTChan centralMessageChan (HighScores.NewUser clientMessageChan)

    -- Run two threads and terminate when either dies.
    -- (1) Process the new message channel responsible for this Client.
    -- (2) Read the WebSocket connection and pass it on to the central
    --     message channel.
    race (HighScores.processClientChan conn clientMessageChan) $ forever $ do
        msg <- WS.receiveData conn
        atomically $ writeTChan centralMessageChan (HighScores.Event msg)
        Tio.putStrLn msg
    return ()  -- We don't care about the results of `race`


fallbackApp :: Application
fallbackApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request."


app :: TChan HighScores.CentralMessage -> Application
app chan = websocketsOr WS.defaultConnectionOptions (wsApp chan) fallbackApp


highScoreServer :: IO ()
highScoreServer = do
    centralMessageChan <- atomically HighScores.newCentralMessageChan
    forkIO $ HighScores.processCentralChan centralMessageChan
    run 8080 (app centralMessageChan)
