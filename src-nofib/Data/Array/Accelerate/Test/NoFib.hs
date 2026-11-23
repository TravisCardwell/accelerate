{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib (

  nofib,
  nofibIngredient,

  module Data.Array.Accelerate.Test.NoFib.Sharing,
  module Data.Array.Accelerate.Test.NoFib.Prelude,
  module Data.Array.Accelerate.Test.NoFib.Imaginary,
  module Data.Array.Accelerate.Test.NoFib.Spectral,
  module Data.Array.Accelerate.Test.NoFib.Issues,
  module Data.Array.Accelerate.Test.NoFib.Misc,

) where

import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import Data.Array.Accelerate.Test.NoFib.Sharing
import Data.Array.Accelerate.Test.NoFib.Prelude
import Data.Array.Accelerate.Test.NoFib.Imaginary
import Data.Array.Accelerate.Test.NoFib.Spectral
import Data.Array.Accelerate.Test.NoFib.Issues
import Data.Array.Accelerate.Test.NoFib.Misc

import Control.Applicative ( (<|>) )
import Test.Tasty.Runners
import Test.Tasty.Runners.AntXML
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.Hedgehog ( HedgehogTestLimit(..) )
import System.Environment
import Test.Tasty


nofib :: RunN -> TestTree -> IO ()
nofib runN test_runq = do
  me <- getProgName
  defaultMainWithIngredients
    [ rerunningTests
        [ nofibIngredient
        , listingTests
        , composeReporters antXMLRunner consoleTestReporter
        ]
    ]
    $ fastMode
    $ testGroup me
        [ test_sharing
        , test_prelude runN
        , test_imaginary runN
        , test_spectral runN
        , test_issues runN
        , test_misc runN
        , test_runq
        ]
  where
    -- When in fast mode, set test limit /unless/ @--hedgehog-tests@ specified
    fastMode :: TestTree -> TestTree
    fastMode testTree = askFast $ \case
      False -> testTree
      True  -> flip adjustOption testTree $ \(HedgehogTestLimit limit) ->
        HedgehogTestLimit (limit <|> Just fastTestLimit)
