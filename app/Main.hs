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
        react @'MessageUpdateEvt $ \(_oldMsg, newMsg, _usr, _member) -> do
          void . tell @T.Text newMsg $ "Hey! I saw what you edited!"

        addCommands $ do
          helpCommand
          command @'[User] "pfp" \ctx u -> do
            Right pfp <- fetchAsset (u ^. #avatar)
            let name = maybe "default.png" assetHashFile (u ^. #avatar % #hash)
                file = CreateMessageAttachment name (Just "Your avatar") pfp
            void $ tell ctx file