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
    it "works in simple case" $ runWD $ do
      testWidget cfg (checkBodyText "One") (checkBodyText "Two") $ do
        prerender_ (text "One") (text "Two")

