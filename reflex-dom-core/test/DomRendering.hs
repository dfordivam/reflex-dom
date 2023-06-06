{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Prelude hiding (fail)
import Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Lens.Operators
import Control.Monad hiding (fail)
import Control.Monad.Catch
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Ref
import Data.Constraint.Extras
import Data.Constraint.Extras.TH
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum (DSum(..), (==>))
import Data.Functor.Identity
import Data.Functor.Misc
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.IORef (IORef)
import Data.List (sort)
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Language.Javascript.JSaddle (syncPoint, liftJSM, eval)
import Language.Javascript.JSaddle.Warp
import Language.Javascript.JMacro
import Network.HTTP.Types (status200)
import Network.Socket
import Network.Wai
import Network.WebSockets
import Reflex.Dom.Core
import Reflex.Dom.Widget.Input (dropdown)
import Reflex.Patch.DMapWithMove
import System.Which (staticWhich)
import qualified Test.HUnit as HUnit
import qualified Test.Hspec as H
import qualified Test.Hspec.Core.Spec as H
import Test.Hspec (xit)
import Test.Hspec.WebDriver hiding (click, uploadFile, WD)
import qualified Test.Hspec.WebDriver as WD
import Test.WebDriver (WD(..))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Dependent.Map as DMap
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified GHCJS.DOM.File as File
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.FilePath as FilePath
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Capabilities as WD

import Test.Util.ChromeFlags
import Test.Util.UnshareNetwork

import Selenium
import WebdriverUtils

chromiumPath :: FilePath
chromiumPath = $(staticWhich "chromium")

seleniumConfig = SeleniumSetupConfig
  { _seleniumSetupConfig_chromiumPath = chromiumPath
  , _seleniumSetupConfig_headless = True
  , _seleniumSetupConfig_seleniumPort = 8000
  }

main :: IO ()
main = withSeleniumSpec seleniumConfig $ \runSession -> hspec $ do
  let cfg = TestWidgetConfig False blank 8001
  describe "tests using webdriver session" $ runSession $ do
    it "modification of attributes happen together" $ runWD $ do
      let
        elemCount = 10 :: Int
        elemName = "check-modify-attr-el" :: Text
        checkJs = tshow $ renderJs [jmacro|
          function checkElmsVisibility() {
            var elms = document.getElementsByName(`(elemName)`);
            if (elms.length != `(elemCount)`) {
              document.getElementById("test-result").innerText = "Test Failed, count mismatch";
              return;
            }
            fun isVisible el { return el.offsetParent === null; };
            var isEvenVisible = isVisible(elms[0]);
            fun performChecks {
              for(var i = 0; i < elms.length; i++) {
                var v = isVisible(elms[i]);
                if (i % 2 == 0) {
                  if (isEvenVisible != v) return false;
                } else {
                  if (isEvenVisible == v) return false;
                }
              };
              return true;
            }
            if (performChecks()) {
              document.getElementById("test-result").innerText = "Test Passed";
              window.requestAnimationFrame(checkElmsVisibility);
            } else {
              document.getElementById("test-result").innerText = "Test Failed";
            }
          }
          window.setTimeout(checkElmsVisibility, 1000);
        |]

        confirmTestPassed = do
          -- Allow the checks to run for a while
          liftIO $ threadDelay (5 * 1000 * 1000)
          shouldContainTextNoRetry "Test Passed" =<< findElemWithRetry (WD.ById "test-result")

      testWidget cfg (pure ()) confirmTestPassed $ prerender_ blank $ do
        tickEv <- tickLossyFromPostBuildTime 0.1
        toggledDynV <- toggle False tickEv
        let
          elN n = do
            let attr = ffor toggledDynV $ \b -> ("name" =: elemName) <> "style" =: (if even n == b
                  then "display: none"
                  else "")
            elDynAttr "p" attr $ text $ tshow n
        elAttr "p" ("id" =: "test-result") blank
        forM_ [1 .. elemCount] elN
        void $ liftJSM $ eval checkJs


tshow :: (Show a) => a -> Text
tshow = (T.pack . show)
