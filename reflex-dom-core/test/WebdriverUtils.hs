{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Utilities for working with Webdriver
--
module WebdriverUtils where

import Control.Monad
import Control.Concurrent
import Control.Monad.Catch (SomeException(..), try)
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Test.Hspec.WebDriver as WD
import Test.WebDriver (WD(..))

deriving instance MonadFail WD

checkBodyText :: Text -> WD ()
checkBodyText = checkTextInTag "body"

checkTextInTag :: Text -> Text -> WD ()
checkTextInTag t expected = do
  e <- findElemWithRetry (WD.ByTag t)
  shouldContainText expected e

shouldContainText :: Text -> WD.Element -> WD ()
shouldContainText t = withRetry . shouldContainTextNoRetry t

shouldContainTextNoRetry :: Text -> WD.Element -> WD ()
shouldContainTextNoRetry t = flip WD.shouldBe t <=< WD.getText

findElemWithRetry :: WD.Selector -> WD WD.Element
findElemWithRetry = withRetry . WD.findElem

withRetry :: forall a. WD a -> WD a
withRetry a = wait 300
  where wait :: Int -> WD a
        wait 0 = a
        wait n = try a >>= \case
          Left (e :: SomeException) -> do
            liftIO $ putStrLn ("(retrying due to " <> show e <> ")") *> threadDelay 100000
            wait $ n - 1
          Right v -> return v
