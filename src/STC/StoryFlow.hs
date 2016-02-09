{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module STC.StoryFlow
    ( StoryFlow(..)
    , StoryFlowM
    , storyFlow
    , telling
    , endStory
    , Options(..)
    , step
    , Story
    , StoryBot
    , StateBot(..)
    , ConfigBot(..)
    , start
    , sendStory
    ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Free.TH
import           Control.Monad.Logger
import           Control.Monad.RWS
import           Control.Monad.Trans
import           Control.Monad.Trans.Free
import qualified Data.IntMap              as IM
import qualified Data.List                as L
import           Data.Text                (Text)
import qualified Data.Text                as T

import           Web.Telegram.API.Bot

type StoryBot = RWST ConfigBot () StateBot (LoggingT IO)

type StoryFlowM = FreeT StoryFlow StoryBot

data ConfigBot = ConfigBot
  { token       :: Token
  , storyFormat :: Text }

data StateBot = StateBot
  { waitingQueue :: TBQueue Update -- por que?
  , connections  :: IM.IntMap (StoryFlowM ()) -- ^ hash chat_id, current flow
  }

data Story = Story
  { author  :: !String
  , title   :: !String
  , content :: StoryFlowM ()
  }

data Options
  = Choice Int
  | Start
  | Error Text
  -- | Save --To save a point of story to return to it in other moment
  deriving (Eq,Show)

-- | Representa el flujo de una historia con multiples elecciones
-- s es la cadena de texto a mostrar y op son la opcion a obtener
data StoryFlow next
  = StoryFlow Text (Options -> next)
  | Telling Text next
  -- | InvalidWay
  | EndStory
  deriving (Functor)

makeFree_ ''StoryFlow

storyFlow :: (MonadFree StoryFlow m) => Text -> m Options
telling :: (MonadFree StoryFlow m) => Text -> m ()
endStory :: (MonadFree StoryFlow m) => m a


step :: Message -> Maybe Options -> StoryFlowM () -> StoryBot (Maybe (StoryFlowM ()))
step msg op sf = do
  cfg <- ask
  fr <- runFreeT sf
  case fr of
    Free f -> case f of
      StoryFlow t next -> do
        case op of
          Just op' -> step msg Nothing (next op')
          Nothing -> do
            return $ Just sf -- Error passing no options -> repeat
      Telling t next -> return $ Just sf
      EndStory -> return $ Just sf
    Pure () -> return Nothing

sendStory :: Message -> StoryFlowM () -> StoryBot (StoryFlowM ())
sendStory msg sf = do
  cfg <- ask
  fr <- runFreeT sf
  case fr of
    Free f -> case f of
      StoryFlow t next -> do
        logInfoN "StoryFlow"
        liftIO $ sendMessage (token cfg) (SendMessageRequest (T.pack . show . chat_id $ chat msg) t (Just Markdown) Nothing Nothing Nothing)
        return sf
      Telling t next -> do
        logInfoN "Telling"
        liftIO $ sendMessage (token cfg) (SendMessageRequest (T.pack . show . chat_id $ chat msg) t (Just Markdown) Nothing Nothing Nothing)
        sendStory msg next
      EndStory -> do
        logInfoN "EndStory"
        liftIO $ sendMessage (token cfg) (SendMessageRequest (T.pack . show . chat_id $ chat msg) "END" (Just Markdown) Nothing Nothing Nothing)
        sendStory msg start
    Pure () -> sendStory msg start

start :: StoryFlowM ()
start = do
  option <- storyFlow
    "Está iniciado el bot escriba /start para reiniciar\n\
    \O puede elegir una de las siguientes opciones:\n\n\
    \ 1.- Elegir una historia\n\
    \ 2.- Escribir una historia\n\
    \ 3.- Editar una historia\n"
  case option of
    Choice 1 -> telling "Eligiendo historia" >> choiceStory
    Choice 2 -> telling "Escribiendo historia"
    Choice 3 -> telling "Editando historia"
    Start -> start
    Error s -> telling ("Esta opcion no esta disponible: " `T.append` s) >> start
    Choice n -> telling ("Esa no es una una posible respuesta" `T.append` T.pack (show n)) >> start

choiceStory :: StoryFlowM ()
choiceStory = do
  option <- storyFlow
    "Las siguientes historias están disponibles"
  endStory

writeStory :: StoryFlowM ()
writeStory = do
  cfg <- lift ask
  option <- storyFlow
    ("Puede introducir la historia via un mensaje de texto con el siguiente formato:" `T.append` storyFormat cfg)
  endStory

editStory :: StoryFlowM ()
editStory = do
  option <- storyFlow
    "bla bla bla"
  telling "blabla"
  endStory
