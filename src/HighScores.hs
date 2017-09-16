{-# LANGUAGE OverloadedStrings #-}
module HighScores
    ( newCentralMessageChan
    , newClientMessageChan
    , processCentralChan
    , processClientChan
    , CentralMessage(..)
    , ClientMessage(..)
    ) where

import           Control.Concurrent.STM (STM, TChan, atomically, newTChan,
                                         readTChan, writeTChan)
import           Control.Monad          (forever)
import qualified Data.Map.Strict        as M'
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Network.WebSockets     as WS

import qualified RandomNames
import qualified Utils


data CentralMessage
    = NewUser (TChan ClientMessage)
    | Event Text


newtype ClientMessage
    = Send Text


newtype Client = Client (TChan ClientMessage)


type Points = Int


type Timestamp = Int


type Score = ( Points, Timestamp )


data State = State
    { clients :: [Client]
    , scores  :: M'.Map Text Score
    }


newCentralMessageChan :: STM (TChan CentralMessage)
newCentralMessageChan =
    newTChan


newClientMessageChan :: STM (TChan ClientMessage)
newClientMessageChan =
    newTChan


processCentralChan :: TChan CentralMessage -> IO ()
processCentralChan chan =
    -- This loop holds the server state and reads from the CentralMessage
    -- channel.
    let
        loop :: State -> IO ()
        loop state =
            atomically (readTChan chan) >>= processCentralMessage state >>= loop

        initial :: State
        initial = State
            { clients = []
            , scores = M'.empty
            }
    in
        loop initial


assignScore :: Text -> Score -> ClientMessage
assignScore username ( points, time ) =
    Send $ T.unwords
        [ "AssignScore" :: Text
        , username
        , T.pack $ show points
        , T.pack $ show time
        ]


assignScore' :: TChan ClientMessage -> Text -> Score -> STM ()
assignScore' chan username score =
    writeTChan chan $ assignScore username score


assignUsername :: Text -> ClientMessage
assignUsername username =
    Send $ T.unwords
        [ "AssignUsername" :: Text
        , username
        ]


assignUsername' :: TChan ClientMessage -> Text -> STM ()
assignUsername' chan username =
    writeTChan chan $ assignUsername username


processCentralMessage :: State -> CentralMessage -> IO State
processCentralMessage state (NewUser chan) = do
    -- A new Client has signed on. We need to add the following to the server
    -- state:
    -- (1) Add their ClientMessage channel for broadcasting future updates.
    -- (2) Add their username as a key to the scoring Dict with a fresh value.
    time <- Utils.getCurrentTime
    username <- RandomNames.getRandomName

    -- Add the new Client to the server state.
    let nextClients = Client chan : clients state

    -- Produce a fresh score and insert it into the score board Dict.
    let newScore = ( 0, time )
    let nextScores = M'.insert username newScore $ scores state

    -- Construct the next state with the new list of Clients and new
    -- Dict of scores.
    let nextState = state { clients = nextClients, scores = nextScores }

    -- Write all the required messages.
    atomically $ do
        -- Tell the requesting client their username.
        assignUsername' chan username

        -- Tell the requesting client all the current scores.
        sequence_ $ M'.mapWithKey (assignScore' chan) $ scores nextState

        -- Tell all clients about the new username with a fresh score.
        mapM_ (\(Client chan) -> assignScore' chan username newScore) $ clients state

    -- Provide the new state back to the loop.
    return nextState

processCentralMessage state (Event username) =
    -- An existing Client has earned a point. We need to:
    -- (1) Modify the scoring Dict in the server state.
    -- (2) Broadcast the new score to all clients.
    case M'.lookup username (scores state) of
        -- This is a bogus request that does not change our state at all.
        Nothing ->
            return state

        Just ( points, _ ) -> do
            -- Construct our next state.
            time <- Utils.getCurrentTime
            let nextScore = ( points + 1, time )
            let nextScores = M'.insert username nextScore $ scores state
            let nextState = state { scores = nextScores }

            atomically $
                -- Tell all clients about the new score.
                mapM_ (\(Client chan) -> assignScore' chan username nextScore) $ clients state
            return nextState


processClientChan :: WS.Connection -> TChan ClientMessage -> IO ()
processClientChan conn chan = forever $ do
    -- This reads a ClientMessage channel forever and passes any messages
    -- it reads to the WebSocket Connection.
    message <- atomically $ readTChan chan
    case message of
      HighScores.Send text ->
          WS.sendTextData conn text
