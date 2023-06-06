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
import Language.Javascript.JSaddle (syncPoint, liftJSM)
import Language.Javascript.JSaddle.Warp
import Network.HTTP.Types (status200)
import Network.Socket
import Network.Wai
import Network.WebSockets
import Reflex.Dom.Core
import Reflex.Dom.Widget.Input (dropdown)
import Reflex.Patch.DMapWithMove
import System.Directory
import System.Environment
import System.IO (stderr)
import System.IO.Silently
import System.IO.Streams (connect)
import System.IO.Streams.File (withFileAsOutput)
import System.IO.Streams.Handle (handleToInputStream)
import qualified System.IO.Streams.Process
import System.IO.Temp
import System.Process
import System.Which (staticWhich)
import qualified Test.HUnit as HUnit
import qualified Test.Hspec as H
import qualified Test.Hspec.Core.Spec as H
import Test.Hspec (xit)
import Test.Hspec.WebDriver hiding (runWD, click, uploadFile, WD)
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


keyMap :: DMap DKey Identity
keyMap = DMap.fromList
  [ Key_Int ==> 0
  , Key_Char ==> 'A'
  ]

data DKey a where
  Key_Int :: DKey Int
  Key_Char :: DKey Char
  Key_Bool :: DKey Bool


textKey :: DKey a -> Text
textKey = \case
  Key_Int -> "Key_Int"
  Key_Char -> "Key_Char"
  Key_Bool -> "Key_Bool"

deriveArgDict ''DKey
deriveGEq ''DKey
deriveGCompare ''DKey
deriveGShow ''DKey

deriving instance MonadFail WD

main :: IO ()
main = do
  unshareNetwork
  isHeadless <- (== Nothing) <$> lookupEnv "NO_HEADLESS"
  withSandboxedChromeFlags isHeadless $ \chromeFlags -> do
    withSeleniumServer $ \selenium -> do
      let browserPath = T.strip $ T.pack chromium
      when (T.null browserPath) $ fail "No browser found"
      withDebugging <- isNothing <$> lookupEnv "NO_DEBUG"
      let wdConfig = WD.defaultConfig { WD.wdPort = fromIntegral $ _selenium_portNumber selenium }
          chromeCaps' = WD.getCaps $ chromeConfig browserPath chromeFlags
      hspec (tests withDebugging wdConfig [(chromeCaps', "chrome")] selenium) `finally` _selenium_stopServer selenium

tests :: Bool -> WD.WDConfig -> [(Capabilities, String)] -> Selenium -> Spec
tests withDebugging wdConfig caps _selenium = do
  let putStrLnDebug :: MonadIO m => Text -> m ()
      putStrLnDebug m = when withDebugging $ liftIO $ putStrLn $ T.unpack m
      session' t = sessionWith wdConfig t . using caps
      runWD m = runWDOptions (WdOptions False) $ do
        putStrLnDebug "before"
        r <- m
        putStrLnDebug "after"
        return r
  session' "text" $ do
    it "can add/update/remove attributes" $ runWD $ do
      let checkInitialAttrs = do
            e <- findElemWithRetry $ WD.ByTag "div"
            assertAttr e "const" (Just "const")
            assertAttr e "delete" (Just "delete")
            assertAttr e "init" (Just "init")
            assertAttr e "click" Nothing
            pure e
          checkModifyAttrs e = do
            WD.click e
            withRetry $ do
              assertAttr e "const" (Just "const")
              assertAttr e "delete" Nothing
              assertAttr e "init" (Just "click")
              assertAttr e "click" (Just "click")
      testWidget' checkInitialAttrs checkModifyAttrs $ mdo
        let conf = def
              & initialAttributes .~ "const" =: "const" <> "delete" =: "delete" <> "init" =: "init"
              & modifyAttributes .~ (("delete" =: Nothing <> "init" =: Just "click" <> "click" =: Just "click") <$ click)
        (e, ()) <- element "div" conf $ text "hello world"
        let click = domEvent Click e
        return ()

