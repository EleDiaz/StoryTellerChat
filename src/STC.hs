{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module STC
    ( telegramBotServer
    ) where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.RWS
import           Data.Attoparsec.Text   (parseOnly)
import qualified Data.IntMap            as IM
import           Data.Maybe
import qualified Data.Text              as T
import qualified Data.Text.IO           as T

import           Web.Telegram.API.Bot

import           STC.StoryFlow
import           STC.StoryParser


telegramBotServer :: T.Text -> IO ()
telegramBotServer tok = do
  queueWaiting <- atomically $ newTBQueue 100 -- Solo 100 mensajes en cola
  let state = StateBot queueWaiting IM.empty
  let config = ConfigBot token ""
  void . runStdoutLoggingT . flip (`runRWST` config) state $ do
    logInfoN "Starting Bot"
    update <- liftIO $ getUpdates token Nothing (Just 1) (Just 20)
    case update of
      Right (UpdatesResponse [upd]) -> do
        logInfoN "First message to Server"
        serves token (update_id upd)
      Left err -> logErrorN $ T.concat
        ["Wrong request!! -> \n", T.pack $ show err]

  where token = Token tok

  -- meter dentro de una variable de TBQueue mientras no este llena
  -- Current maximum length is 4096 UTF8 characters -- de un mensaje de text
  -- 16KB
  -- tener una cola de 10.000 ~> 156.4 MB pensando en el peor caso

-- | Se tiene limitado la cantidad de mensajes a recibir por parte del telegram
-- a 100. Este va estar iterando de forma infinita para obtener todos los
-- mensajes que resten.
serves :: Token -> Int -> StoryBot ()
serves tok offset = do
  respond <- liftIO $ getUpdates tok (Just offset) (Just 100) (Just 20)
  case respond of
    Right (UpdatesResponse []) -> do
      logInfoN "Waiting for messages"
      serves tok offset -- Continue waiting for messages
    Right (UpdatesResponse ups) -> do
      let offset' = foldl (\s a -> max s (update_id a)) 0 ups
      logInfoN  $ T.concat
        ["A new pack of messages to Server with offset: ", T.pack $ show offset]

      forM_ ups $ \upd -> do
        st <- get
        case message upd of
          Nothing -> logWarnN $ T.concat
                  ["Something weird -> ", T.pack $ show upd]
          Just msg ->
            case IM.lookup (chat_id . chat $ msg) (connections st) of
              Just a -> parseMessage msg a
              Nothing -> addConnection msg

      serves tok (offset' + 1)
    Left err ->  logErrorN $ T.concat
      ["Wrong request!! -> \n", T.pack $ show err]

parseMessage :: Message -> StoryFlowM () -> StoryBot ()
parseMessage msg sfM =
  case text msg of
    Just txt -> do
      let option = parseOnly optionsParser txt
      cfg <- ask
      case either (Error . T.pack) id option of -- HERE: Se añaden nuevos comandos y aqui se controlan
        Start -> sendStory msg start >>= newStory -- Restart menu
        op -> do
          mSF <- step msg (Just op) sfM
          case mSF of
            Just sf -> sendStory msg sf >>= newStory
            Nothing -> sendStory msg start >>= newStory -- End story
    Nothing ->
      logWarnN $ T.concat
            ["User sending invalid option: ", T.pack $ show msg]
  where
    newStory sf = modify (\st -> st { connections = IM.alter (const $ Just sf) (chat_id $ chat msg) (connections st)})


addConnection :: Message -> StoryBot ()
addConnection msg = do
  sf <- sendStory msg start
  modify (\st -> st { connections = IM.insert (chat_id $ chat msg) sf (connections st)})
