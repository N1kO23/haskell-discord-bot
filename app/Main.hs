{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import Calamity
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Commands.Context (FullContext (FullContext), useFullContext)
import qualified Calamity.Interactions as I
import Calamity.Metrics.Noop
import Calamity.Utils.CDNUrl (assetHashFile)
import Control.Concurrent
import Control.Monad
import LoadEnv
import qualified Data.Text as T
import qualified Di
import qualified DiPolysemy as DiP
import Optics
import qualified Polysemy as P
import qualified Polysemy.Async as P
import qualified Polysemy.State as P
import System.Environment (getEnv)
import TextShow
import Calamity (Embed(author), Message (author), ExecuteWebhookOptions (avatarUrl))
import GHC.IO.Device (IODevice(ready))
import Calamity.Cache.Eff (getGuilds)
import Calamity.Gateway
import Data.Text.Lazy (append)
import Calamity.Commands (CommandInvoked(ctx))

data MyViewState = MyViewState
  { numOptions :: Int,
    selected :: Maybe T.Text
  }

$(makeFieldLabelsNoPrefix ''MyViewState)

main :: IO ()
main = do
  loadEnv
  loginToken <- T.pack <$> getEnv "BOT_TOKEN"
  Di.new $ \di ->
    void . P.runFinal . P.embedToFinal . DiP.runDiToIO di
      . runCacheInMemory
      . runMetricsNoop
      . useFullContext
      . useConstantPrefix ">"
      . runBotIO (BotToken loginToken) defaultIntents
      $ do
        DiP.info @T.Text "Setting up commands and handlers..."

        react @'ReadyEvt $ \ready-> do
          DiP.info @T.Text "The bot is ready!"
          let activity = Activity "haskelling" Game Nothing Nothing Nothing (Just "Running on pure functional language like a boss") Nothing Nothing Nothing Nothing Nothing Nothing
              curPrecense = StatusUpdateData Nothing [activity] Idle True
          void $ sendPresence curPrecense


        react @'MessageUpdateEvt $ \(_oldMsg, newMsg, _usr, _member) -> do
          void . tell @T.Text newMsg $ "Hey! I saw what you edited!"

        react @'MessageCreateEvt $ \(msg, _usr, _member) -> do
          when ("Haskell" `T.isInfixOf` (msg ^. #content)) $ 
            void . invoke $ CreateReaction msg msg (UnicodeEmoji "😍")

        addCommands $ do
          helpCommand
          command @'[User] "pfp" \ctx u -> do
            Right pfp <- fetchAsset (u ^. #avatar)
            let name = maybe "default.png" assetHashFile (u ^. #avatar % #hash)
                file = CreateMessageAttachment name (Just "Your avatar") pfp
            void $ tell ctx file