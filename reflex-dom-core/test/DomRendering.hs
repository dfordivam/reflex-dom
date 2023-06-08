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
import Control.Lens hiding (has)
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
  describe "Rendering of DOM updates" $ runSession $ do
    it "modification of attributes happen together" $ runWD $ do
      let
        elemCount = 10 :: Int
        elemName = "check-modify-attr-el" :: Text
        -- Checks that even and odd elems have same attributes as they are
        -- derived from a single Dynamic value. In this case it is visibility,
        -- so either even numbers or odd numbers should be visible at one point
        -- in time
        checkJs = tshow $ renderJs [jmacro|
          function performChecksInAnimationFrame() {
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
              window.requestAnimationFrame(performChecksInAnimationFrame);
            } else {
              document.getElementById("test-result").innerText = "Test Failed";
            }
          }
          window.setTimeout(performChecksInAnimationFrame, 1000);
        |]

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

    it "modification of child widgets happen together" $ runWD $ do
      let
        elemCount = 10 :: Int
        elemName = "elemName" :: Text
        -- Checks that all the child elements of runWithReplace get updated
        -- together as they all get updated via a single Event
        checkJs = tshow $ renderJs [jmacro|
          function performChecksInAnimationFrame() {
            var elms = document.getElementsByName(`(elemName)`);
            if (elms.length != `(elemCount)`) {
              document.getElementById("test-result").innerText = "Test Failed, count mismatch";
              return;
            }
            fun performChecks {
              for(var i = 0; i < elms.length; i++) {
                if (elms[i].innerText != elms[0].innerText) {
                  return false;
                }
              };
              return true;
            }
            if (performChecks()) {
              document.getElementById("test-result").innerText = "Test Passed";
              window.requestAnimationFrame(performChecksInAnimationFrame);
            } else {
              document.getElementById("test-result").innerText = "Test Failed";
            }
          }
          window.setTimeout(performChecksInAnimationFrame, 1000);
        |]

      testWidget cfg (pure ()) confirmTestPassed $ prerender_ blank $ do
        tickEv <- tickLossyFromPostBuildTime 0.1
        let
          elN n = do
            elAttr "p" ("name" =: elemName) $ do
              runWithReplace blank $ ffor tickEv $ \v -> do
                text $ tshow . _tickInfo_n $ v
        elAttr "p" ("id" =: "test-result") blank
        forM_ [1 .. elemCount] elN
        void $ liftJSM $ eval checkJs

    it "widgets render only after getting ready" $ runWD $ do
      let
        elemCount = 10 :: Int
        elemName = "elemName" :: Text
        unreadyChildName = "unready-child-name" :: Text
        -- Checks that element having unready child is not rendered
        checkJs = tshow $ renderJs [jmacro|
          function performChecksInAnimationFrame() {
            var elms = document.getElementsByName(`(elemName)`);
            if (elms.length != `(elemCount)`) {
              document.getElementById("test-result").innerText = "Test Failed, count mismatch";
              return;
            }
            fun performChecks {
              var unreadyChild = document.getElementsByName(`(unreadyChildName)`);
              if (unreadyChild.length != 0) {
                 return false;
              }
              for(var i = 0; i < elms.length; i++) {
                if (elms[i].innerText != elms[0].innerText) {
                  return false;
                }
              };
              return true;
            }
            if (performChecks()) {
              document.getElementById("test-result").innerText = "Test Passed";
              window.requestAnimationFrame(performChecksInAnimationFrame);
            } else {
              document.getElementById("test-result").innerText = "Test Failed";
            }
          }
          window.setTimeout(performChecksInAnimationFrame, 1000);
        |]

      testWidget cfg (pure ()) confirmTestPassed $ prerender_ blank $ do
        tickEv <- tickLossyFromPostBuildTime 0.1
        let
          elN n = do
            elAttr "p" ("name" =: elemName) $ do
              runWithReplace blank $ ffor tickEv $ \v -> do
                delayedPb <- delay 0.25 =<< getPostBuild
                runWithReplace unreadyChild $ ffor delayedPb $ \_ -> do
                  text $ tshow . _tickInfo_n $ v
          unreadyChild = do
            elAttr "div" ("name" =: unreadyChildName) notReady
        elAttr "p" ("id" =: "test-result") blank
        forM_ [1 .. elemCount] elN
        void $ liftJSM $ eval checkJs

    it "DMap patches render together" $ runWD $ do
      let
        elemCount = 10 :: Int
        elemName = "elemName" :: Text
        unreadyChildName = "unready-child-name" :: Text

        -- Checks that the sequence of patches when applied leads to the expected DOM updates
        -- The current patch count is specified in Key_1, which allows the JS code to do the verification

        initialDMap :: DMap DKey Identity
        initialDMap = DMap.fromList
          [ Key_1 ==> 0
          , Key_2 ==> 'a'
          , Key_3 ==> False
          ]

        -- The patches have been chosen such that they repeat exactly after one cycle
        patches = IntMap.fromList
          [ (1, patch1)
          , (2, patch2)
          , (3, patch3)
          , (4, patch4)
          ]
        patch1 = insertDMapKey Key_1 (Identity 1)
          <> insertDMapKey Key_4 (Identity "V4")
          <> insertDMapKey Key_5 (Identity "V5a")
        patch2 = insertDMapKey Key_1 (Identity 2)
          <> insertDMapKey Key_2 (Identity 'b')
          <> insertDMapKey Key_3 (Identity True)
          <> moveDMapKey Key_4 Key_5
        patch3 = insertDMapKey Key_1 (Identity 3)
          <> insertDMapKey Key_5 (Identity "V5b")
          <> moveDMapKey Key_5 Key_4
        patch4 = insertDMapKey Key_1 (Identity 4)
          <> insertDMapKey Key_2 (Identity 'a')
          <> deleteDMapKey Key_4
          <> deleteDMapKey Key_5
          <> insertDMapKey Key_3 (Identity False)

        -- Contents of DMap after patch application
        mkVals = DMap.foldrWithKey (\k (Identity v) ls -> (textKey k, T.pack $ has @Show k $ show v):ls) []
        vals :: [[(Text, Text)]]
        vals = [vals0, vals1, vals2, vals3, vals4]
        vals0 = mkVals initialDMap
        vals1 = mkVals (applyAlways patch1 initialDMap)
        -- A bit of fixup of the values, as the move of widget would cause the
        -- Key_5 to still contain the text "Key_4"
        vals2 = elementOf (traverse . _1) 3 .~ "Key_4" $
          (mkVals (applyAlways (patch2 <> patch1) initialDMap))
        vals3 = mkVals (applyAlways (patch3 <> patch2 <> patch1) initialDMap)
        vals4 = mkVals (applyAlways (patch4 <> patch3 <> patch2 <> patch1) initialDMap)

        checkJs = tshow $ renderJs [jmacro|
          function performChecksInAnimationFrame() {
            var elms = document.getElementsByName(`(elemName)`);
            if (elms.length != `(elemCount)`) {
              document.getElementById("test-result").innerText = "Test Failed, count mismatch";
              return;
            }
            fun performChecks {
              var unreadyChild = document.getElementsByName(`(unreadyChildName)`);
              if (unreadyChild.length != 0) {
                 console.log("found unreadyChildName");
                 return false;
              }
              function checkEl (e, vals) {
                if (e.childElementCount != vals.length) {
                 console.log(e.childElementCount, vals.length);
                return false;
                }
                for (var i = 0; i < vals.length; i++) {
                  if (e.children[i].children[0].innerText != vals[i][0]) {
                    for (var i = 0; i < vals.length; i++) {
                       console.log(e.children[i].children[0].innerText, vals[i][0]);
                       console.log(e.children[i].children[1].innerText, vals[i][1]);
                    }
                    return false;
                  }
                  if (e.children[i].children[1].innerText != vals[i][1]) {
                    for (var i = 0; i < vals.length; i++) {
                       console.log(e.children[i].children[0].innerText, vals[i][0]);
                       console.log(e.children[i].children[1].innerText, vals[i][1]);
                    }
                    return false;
                  }
                }
                return true;
              }
              var js_vals = `(vals)`;
              for(var i = 0; i < elms.length; i++) {
                // Due to the notReady, each of the elms may be showing a
                // different patch. Get the patch for the current el via "Key_1" value
                var currentPatch = Number(elms[i].children[0].children[1].innerText);
                if (!checkEl(elms[i], js_vals[currentPatch])) {
                  return false;
                }
              };
              return true;
            }
            if (performChecks()) {
              document.getElementById("test-result").innerText = "Test Passed";
              window.requestAnimationFrame(performChecksInAnimationFrame);
            } else {
              document.getElementById("test-result").innerText = "Test Failed";
            }
          }
          window.setTimeout(performChecksInAnimationFrame, 1000);
        |]

      testWidget cfg (pure ()) confirmTestPassed $ prerender_ blank $ do
        tickEv <- (fmap _tickInfo_n) <$> tickLossyFromPostBuildTime 0.5
        -- tickEv <- (fmap fst) <$> (numberOccurrences =<< button "Tick Increment")
        let
          -- curPatchEv :: Event t Int
          curPatchEv = ffor tickEv $ \n -> (rem (fromIntegral n) 4) + 1

          patchEv = fforMaybe curPatchEv $ \n -> IntMap.lookup n patches

          widget :: (DomBuilder t m, NotReady t m, PerformEvent t m, TriggerEvent t m, PostBuild t m, MonadIO (Performable m)) => DKey a -> Identity a -> m (Identity a)
          widget k (Identity v) = elAttr "li" ("id" =: textKey k) $ do
            elClass "span" "key" $ text $ textKey k
            let
              vTxt :: Text
              vTxt = T.pack $ has @Show k $ show v
            elClass "span" "value" $ text vTxt
            -- hack to make one of the widget notReady
            when (vTxt == "True") $ do
              delayedPb <- delay 0.25 =<< getPostBuild
              void $ runWithReplace unreadyChild $ ffor delayedPb $ \_ -> blank
            pure (Identity v)

          elN n = do
            void $ elAttr "p" ("name" =: elemName) $ do
              traverseDMapWithKeyWithAdjustWithMove widget initialDMap patchEv

          unreadyChild :: (DomBuilder t m, NotReady t m) => m ()
          unreadyChild = do
            elAttr "div" ("name" =: unreadyChildName) notReady

        elAttr "p" ("id" =: "test-result") blank
        forM_ [1 .. elemCount] elN
        void $ liftJSM $ eval checkJs

confirmTestPassed = do
  let testRunDuration = (5 * 1000 * 1000)
  -- Allow the checks to run for a while
  liftIO $ threadDelay testRunDuration
  shouldContainTextNoRetry "Test Passed" =<< findElemWithRetry (WD.ById "test-result")

tshow :: (Show a) => a -> Text
tshow = (T.pack . show)

data DKey a where
  Key_1 :: DKey Int
  Key_2 :: DKey Char
  Key_3 :: DKey Bool
  Key_4 :: DKey Text
  Key_5 :: DKey Text

textKey :: DKey a -> Text
textKey = \case
  Key_1 -> "Key_1"
  Key_2 -> "Key_2"
  Key_3 -> "Key_3"
  Key_4 -> "Key_4"
  Key_5 -> "Key_5"

deriveArgDict ''DKey
deriveGEq ''DKey
deriveGCompare ''DKey
deriveGShow ''DKey
