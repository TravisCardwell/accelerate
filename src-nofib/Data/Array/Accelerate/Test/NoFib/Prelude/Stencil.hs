-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Prelude.Stencil
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Prelude.Stencil (

  test_stencil

) where

import Data.Typeable
import Prelude                                                      as P

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Sugar.Elt                              as S
import Data.Array.Accelerate.Sugar.Array                            as S
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import Data.Array.Accelerate.Test.Similar

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Test.Tasty
import Test.Tasty.Hedgehog


test_stencil :: RunN -> TestTree
test_stencil runN = askFast $ \fast ->
  testGroup "stencil"
    [ at @TestInt8   $ testElt fast i8
    , at @TestInt16  $ testElt fast i16
    , at @TestInt32  $ testElt fast i32
    , at @TestInt64  $ testElt fast i64
    , at @TestWord8  $ testElt fast w8
    , at @TestWord16 $ testElt fast w16
    , at @TestWord32 $ testElt fast w32
    , at @TestWord64 $ testElt fast w64
    , at @TestHalf   $ testElt fast f16
    , at @TestFloat  $ testElt fast f32
    , at @TestDouble $ testElt fast f64
    ]
  where
    testElt
        :: forall e. (A.Num e, P.Num e, Show e, Similar e)
        => Bool  -- ^ Fast mode?
        -> Gen e
        -> TestTree
    testElt fast e =
      testGroup (show (eltR @e)) $
        [ testDim1
        , testDim2
        , testDim3
        ]
        P.++
        -- More extensive tests do not run in fast mode because a kernel is
        -- compiled for each test case.
        [ testStatic | P.not fast ]
      where
        (max1, max2, max3)
          | fast      = ( 64, 24, 12)
          | otherwise = (256, 96, 32)

        testDim1 :: TestTree
        testDim1 =
          testGroup "DIM1"
            [ testProperty "stencil3" $
                prop_stencil runN 3 (dim1In 2 max1) e flatten3 flatten3 stencil3Ref
            , testProperty "stencil5" $
                prop_stencil runN 5 (dim1In 3 max1) e flatten5 flatten5 stencil5Ref
            , testProperty "stencil7" $
                prop_stencil runN 7 (dim1In 4 max1) e flatten7 flatten7 stencil7Ref
            , testProperty "stencil9" $
                prop_stencil runN 9 (dim1In 5 max1) e flatten9 flatten9 stencil9Ref
            ]

        testDim2 :: TestTree
        testDim2 =
          testGroup "DIM2"
            [ testProperty "stencil3x3" $
                prop_stencil runN  9 (dim2In 2 max2) e flatten3x3 flatten3x3 stencil3x3Ref
            , testProperty "stencil5x5" $
                prop_stencil runN 25 (dim2In 3 max2) e flatten5x5 flatten5x5 stencil5x5Ref
            , testProperty "stencil7x7" $
                prop_stencil runN 49 (dim2In 4 max2) e flatten7x7 flatten7x7 stencil7x7Ref
            , testProperty "stencil9x9" $
                prop_stencil runN 81 (dim2In 5 max2) e flatten9x9 flatten9x9 stencil9x9Ref
            ]

        testDim3 :: TestTree
        testDim3 =
          testGroup "DIM3"
            [ testProperty "stencil3x3x3" $
                prop_stencil runN 27 (dim3In 2 max3) e flatten3x3x3 flatten3x3x3 stencil3x3x3Ref
            ]

        testStatic :: TestTree
        testStatic =
          let conf = withTests 10
          in  testGroup "static"
                [ testProperty "stencil3"     $ conf $ test_stencil3     runN e
                , testProperty "stencil5"     $ conf $ test_stencil5     runN e
                , testProperty "stencil7"     $ conf $ test_stencil7     runN e
                , testProperty "stencil9"     $ conf $ test_stencil9     runN e
                , testProperty "stencil3x3"   $ conf $ test_stencil3x3   runN e
                , testProperty "stencil5x5"   $ conf $ test_stencil5x5   runN e
                , testProperty "stencil7x7"   $ conf $ test_stencil7x7   runN e
                , testProperty "stencil9x9"   $ conf $ test_stencil9x9   runN e
                , testProperty "stencil3x3x3" $ conf $ test_stencil3x3x3 runN e
                ]

{-------------------------------------------------------------------------------
  Compile once
-------------------------------------------------------------------------------}

-- | Kernels for a given shape and element type
--
-- The stencil function sums elements selected by a boolean mask over the
-- flattened (row-major) stencil neighbourhood.
--
-- The fields are lazy, so each kernel is compiled on first use.
data Kernels sh e = Kernels
  { kClamp    :: Vector Bool             -> Array sh e -> Array sh e
  , kWrap     :: Vector Bool             -> Array sh e -> Array sh e
  , kMirror   :: Vector Bool             -> Array sh e -> Array sh e
  , kConstant :: Vector Bool -> Scalar e -> Array sh e -> Array sh e
  }

-- | Construct 'Kernels'
kernels
  :: forall sh e stencil. (A.Num e, Shape sh, Stencil sh e stencil)
  => RunN
  -> (stencil -> [Exp e])  -- ^ Flattening function
  -> Kernels sh e
kernels runN flatten' = Kernels
  { kClamp    = runN $ \mask   xs -> A.stencil (masked mask) A.clamp  xs
  , kWrap     = runN $ \mask   xs -> A.stencil (masked mask) A.wrap   xs
  , kMirror   = runN $ \mask   xs -> A.stencil (masked mask) A.mirror xs
  , kConstant = runN $ \mask c xs ->
      let fn = A.function $ \_ -> A.the c
      in  A.stencil (masked mask) fn xs
  }
  where
    masked :: Acc (Vector Bool) -> stencil -> Exp e
    masked mask st = P.sum
      [ (mask A.! I1 (constant i)) ? (x, 0)
      | (i, x) <- P.zip [0..] (flatten' st)
      ]

-- | Stencil function reference implementation
maskedRef :: P.Num e => [Bool] -> [e] -> e
maskedRef mask xs = P.sum
  [ if b then x else 0
  | (b, x) <- P.zip mask xs
  ]

-- | Run a 'Kernels' kernel with the given selection mask
runKernels
  :: Elt e
  => Kernels sh e
  -> [Bool]            -- ^ Selection mask
  -> SimpleBoundary e  -- ^ Boundary condition
  -> Array sh e
  -> Array sh e
runKernels k mask = \case
    Clamp      -> kClamp    k mask'
    Wrap       -> kWrap     k mask'
    Mirror     -> kMirror   k mask'
    Constant c -> kConstant k mask' (S.fromList Z [c])
  where
    mask' :: Vector Bool
    mask' = S.fromList (Z :. P.length mask) mask

{-# NOINLINE prop_stencil #-}
prop_stencil
  :: forall sh e stencil stencilRef.
     ( A.Num e, P.Num e, Show e, Similar e
     , P.Eq sh, Shape sh, Show sh
     , Stencil sh e stencil
     )
  => RunN
  -> Int                   -- ^ Number of elements in the stencil
  -> Gen sh                -- ^ Shape of the input array
  -> Gen e
  -> (stencil -> [Exp e])  -- ^ Flatten the stencil neighborhood
  -> (stencilRef -> [e])   -- ^ Flatten (for reference implementation)
  -> ((stencilRef -> e) -> SimpleBoundary e -> Array sh e -> Array sh e)
  -- ^ Reference implementation
  -> Property
prop_stencil runN n shape' e flatten' flattenRef ref =
  property $ do
    sh   <- forAll shape'
    xs   <- forAll (array sh e)
    b    <- forAll (boundary e)
    mask <- forAll (Gen.list (Range.singleton n) Gen.bool)
    --
    runKernels k mask b xs ~~~ ref (maskedRef mask . flattenRef) b xs
  where
    k :: Kernels sh e
    k = kernels runN flatten'

flatten3 :: (a,a,a) -> [a]
flatten3 (x0,x1,x2) = [x0,x1,x2]

flatten5 :: (a,a,a,a,a) -> [a]
flatten5 (x0,x1,x2,x3,x4) = [x0,x1,x2,x3,x4]

flatten7 :: (a,a,a,a,a,a,a) -> [a]
flatten7 (x0,x1,x2,x3,x4,x5,x6) = [x0,x1,x2,x3,x4,x5,x6]

flatten9 :: (a,a,a,a,a,a,a,a,a) -> [a]
flatten9 (x0,x1,x2,x3,x4,x5,x6,x7,x8) = [x0,x1,x2,x3,x4,x5,x6,x7,x8]

flatten3x3 :: ((a,a,a),(a,a,a),(a,a,a)) -> [a]
flatten3x3 = P.concatMap flatten3 . flatten3

flatten5x5 :: ((a,a,a,a,a),(a,a,a,a,a),(a,a,a,a,a),(a,a,a,a,a),(a,a,a,a,a)) -> [a]
flatten5x5 = P.concatMap flatten5 . flatten5

flatten7x7
  :: ( (a,a,a,a,a,a,a), (a,a,a,a,a,a,a), (a,a,a,a,a,a,a), (a,a,a,a,a,a,a)
     , (a,a,a,a,a,a,a), (a,a,a,a,a,a,a), (a,a,a,a,a,a,a)
     )
  -> [a]
flatten7x7 = P.concatMap flatten7 . flatten7

flatten9x9
  :: ( (a,a,a,a,a,a,a,a,a), (a,a,a,a,a,a,a,a,a), (a,a,a,a,a,a,a,a,a)
     , (a,a,a,a,a,a,a,a,a), (a,a,a,a,a,a,a,a,a), (a,a,a,a,a,a,a,a,a)
     , (a,a,a,a,a,a,a,a,a), (a,a,a,a,a,a,a,a,a), (a,a,a,a,a,a,a,a,a)
     )
  -> [a]
flatten9x9 = P.concatMap flatten9 . flatten9

flatten3x3x3
  :: ( ((a,a,a),(a,a,a),(a,a,a))
     , ((a,a,a),(a,a,a),(a,a,a))
     , ((a,a,a),(a,a,a),(a,a,a))
     )
  -> [a]
flatten3x3x3 = P.concatMap flatten3x3 . flatten3

{-------------------------------------------------------------------------------
  Compile per test case
-------------------------------------------------------------------------------}

{-# NOINLINE test_stencil3 #-}
test_stencil3
    :: (P.Num e, A.Num e, Similar e, Show e)
    => RunN
    -> Gen e
    -> Property
test_stencil3 runN e =
  property $ do
    sh        <- forAll ((Z :.) P.<$> Gen.int (Range.linear 2 256))
    xs        <- forAll (array sh e)
    b         <- forAll (boundary e)
    P3 _ a r  <- forAll pattern3
    let !go = case b of
                Clamp      -> runN (A.stencil a A.clamp)
                Wrap       -> runN (A.stencil a A.wrap)
                Mirror     -> runN (A.stencil a A.mirror)
                Constant x -> runN (A.stencil a (A.function (\_ -> constant x)))
    --
    go xs ~~~ stencil3Ref r b xs

{-# NOINLINE test_stencil5 #-}
test_stencil5
    :: (P.Num e, A.Num e, Similar e, Show e)
    => RunN
    -> Gen e
    -> Property
test_stencil5 runN e =
  property $ do
    sh        <- forAll ((Z :.) P.<$> Gen.int (Range.linear 3 256))
    xs        <- forAll (array sh e)
    b         <- forAll (boundary e)
    P5 _ a r  <- forAll pattern5
    let !go = case b of
                Clamp      -> runN (A.stencil a A.clamp)
                Wrap       -> runN (A.stencil a A.wrap)
                Mirror     -> runN (A.stencil a A.mirror)
                Constant x -> runN (A.stencil a (A.function (\_ -> constant x)))
    --
    go xs ~~~ stencil5Ref r b xs

{-# NOINLINE test_stencil7 #-}
test_stencil7
    :: (P.Num e, A.Num e, Similar e, Show e)
    => RunN
    -> Gen e
    -> Property
test_stencil7 runN e =
  property $ do
    sh        <- forAll ((Z :.) P.<$> Gen.int (Range.linear 4 256))
    xs        <- forAll (array sh e)
    b         <- forAll (boundary e)
    P7 _ a r  <- forAll pattern7
    let !go = case b of
                Clamp      -> runN (A.stencil a A.clamp)
                Wrap       -> runN (A.stencil a A.wrap)
                Mirror     -> runN (A.stencil a A.mirror)
                Constant x -> runN (A.stencil a (A.function (\_ -> constant x)))
    --
    go xs ~~~ stencil7Ref r b xs

{-# NOINLINE test_stencil9 #-}
test_stencil9
    :: (P.Num e, A.Num e, Similar e, Show e)
    => RunN
    -> Gen e
    -> Property
test_stencil9 runN e =
  property $ do
    sh        <- forAll ((Z :.) P.<$> Gen.int (Range.linear 5 256))
    xs        <- forAll (array sh e)
    b         <- forAll (boundary e)
    P9 _ a r  <- forAll pattern9
    let !go = case b of
                Clamp      -> runN (A.stencil a A.clamp)
                Wrap       -> runN (A.stencil a A.wrap)
                Mirror     -> runN (A.stencil a A.mirror)
                Constant x -> runN (A.stencil a (A.function (\_ -> constant x)))
    --
    go xs ~~~ stencil9Ref r b xs


{-# NOINLINE test_stencil3x3 #-}
test_stencil3x3
    :: (P.Num e, A.Num e, Similar e, Show e)
    => RunN
    -> Gen e
    -> Property
test_stencil3x3 runN e =
  property $ do
    sy <- forAll (Gen.int (Range.linear 2 96))
    sx <- forAll (Gen.int (Range.linear 2 96))
    let sh = Z :. sy :. sx
    xs          <- forAll (array sh e)
    b           <- forAll (boundary e)
    P3x3 _ a r  <- forAll pattern3x3
    let !go = case b of
                Clamp      -> runN (A.stencil a A.clamp)
                Wrap       -> runN (A.stencil a A.wrap)
                Mirror     -> runN (A.stencil a A.mirror)
                Constant x -> runN (A.stencil a (A.function (\_ -> constant x)))
    --
    go xs ~~~ stencil3x3Ref r b xs

{-# NOINLINE test_stencil5x5 #-}
test_stencil5x5
    :: (P.Num e, A.Num e, Similar e, Show e)
    => RunN
    -> Gen e
    -> Property
test_stencil5x5 runN e =
  property $ do
    sy <- forAll (Gen.int (Range.linear 3 96))
    sx <- forAll (Gen.int (Range.linear 3 96))
    let sh = Z :. sy :. sx
    xs          <- forAll (array sh e)
    b           <- forAll (boundary e)
    P5x5 _ a r  <- forAll pattern5x5
    let !go = case b of
                Clamp      -> runN (A.stencil a A.clamp)
                Wrap       -> runN (A.stencil a A.wrap)
                Mirror     -> runN (A.stencil a A.mirror)
                Constant x -> runN (A.stencil a (A.function (\_ -> constant x)))
    --
    go xs ~~~ stencil5x5Ref r b xs

{-# NOINLINE test_stencil7x7 #-}
test_stencil7x7
    :: (P.Num e, A.Num e, Similar e, Show e)
    => RunN
    -> Gen e
    -> Property
test_stencil7x7 runN e =
  property $ do
    sy <- forAll (Gen.int (Range.linear 4 96))
    sx <- forAll (Gen.int (Range.linear 4 96))
    let sh = Z :. sy :. sx
    xs          <- forAll (array sh e)
    b           <- forAll (boundary e)
    P7x7 _ a r  <- forAll pattern7x7
    let !go = case b of
                Clamp      -> runN (A.stencil a A.clamp)
                Wrap       -> runN (A.stencil a A.wrap)
                Mirror     -> runN (A.stencil a A.mirror)
                Constant x -> runN (A.stencil a (A.function (\_ -> constant x)))
    --
    go xs ~~~ stencil7x7Ref r b xs

{-# NOINLINE test_stencil9x9 #-}
test_stencil9x9
    :: (P.Num e, A.Num e, Similar e, Show e)
    => RunN
    -> Gen e
    -> Property
test_stencil9x9 runN e =
  property $ do
    sy <- forAll (Gen.int (Range.linear 5 96))
    sx <- forAll (Gen.int (Range.linear 5 96))
    let sh = Z :. sy :. sx
    xs          <- forAll (array sh e)
    b           <- forAll (boundary e)
    P9x9 _ a r  <- forAll pattern9x9
    let !go = case b of
                Clamp      -> runN (A.stencil a A.clamp)
                Wrap       -> runN (A.stencil a A.wrap)
                Mirror     -> runN (A.stencil a A.mirror)
                Constant x -> runN (A.stencil a (A.function (\_ -> constant x)))
    --
    go xs ~~~ stencil9x9Ref r b xs

{-# NOINLINE test_stencil3x3x3 #-}
test_stencil3x3x3
    :: (P.Num e, A.Num e, Similar e, Show e)
    => RunN
    -> Gen e
    -> Property
test_stencil3x3x3 runN e =
  property $ do
    sz <- forAll (Gen.int (Range.linear 2 32))
    sy <- forAll (Gen.int (Range.linear 2 32))
    sx <- forAll (Gen.int (Range.linear 2 32))
    let sh = Z :. sz :. sy :. sx
    xs            <- forAll (array sh e)
    b             <- forAll (boundary e)
    P3x3x3 _ a r  <- forAll pattern3x3x3
    let !go = case b of
                Clamp      -> runN (A.stencil a A.clamp)
                Wrap       -> runN (A.stencil a A.wrap)
                Mirror     -> runN (A.stencil a A.mirror)
                Constant x -> runN (A.stencil a (A.function (\_ -> constant x)))
    --
    go xs ~~~ stencil3x3x3Ref r b xs


type Stencil3Ref a = (a,a,a)
type Stencil5Ref a = (a,a,a,a,a)
type Stencil7Ref a = (a,a,a,a,a,a,a)
type Stencil9Ref a = (a,a,a,a,a,a,a,a,a)

type Stencil3x3Ref a = (Stencil3Ref a, Stencil3Ref a, Stencil3Ref a)
type Stencil5x5Ref a = (Stencil5Ref a, Stencil5Ref a, Stencil5Ref a, Stencil5Ref a, Stencil5Ref a)
type Stencil7x7Ref a = (Stencil7Ref a, Stencil7Ref a, Stencil7Ref a, Stencil7Ref a, Stencil7Ref a, Stencil7Ref a, Stencil7Ref a)
type Stencil9x9Ref a = (Stencil9Ref a, Stencil9Ref a, Stencil9Ref a, Stencil9Ref a, Stencil9Ref a, Stencil9Ref a, Stencil9Ref a, Stencil9Ref a, Stencil9Ref a)

type Stencil3x3x3Ref a = (Stencil3x3Ref a, Stencil3x3Ref a, Stencil3x3Ref a)

type Stencil7x7 a = (Stencil7 a, Stencil7 a, Stencil7 a, Stencil7 a, Stencil7 a, Stencil7 a, Stencil7 a)
type Stencil9x9 a = (Stencil9 a, Stencil9 a, Stencil9 a, Stencil9 a, Stencil9 a, Stencil9 a, Stencil9 a, Stencil9 a, Stencil9 a)


data SimpleBoundary e
  = Wrap
  | Clamp
  | Mirror
  | Constant e
  deriving (P.Eq, Show)

boundary :: Gen e -> Gen (SimpleBoundary e)
boundary e =
  Gen.choice
    [ Constant P.<$> e
    , pure Clamp
    , pure Wrap
    , pure Mirror
    ]

data Pattern3 a = P3 [Int] (Stencil3 a -> Exp a) (Stencil3Ref a -> a)
data Pattern5 a = P5 [Int] (Stencil5 a -> Exp a) (Stencil5Ref a -> a)
data Pattern7 a = P7 [Int] (Stencil7 a -> Exp a) (Stencil7Ref a -> a)
data Pattern9 a = P9 [Int] (Stencil9 a -> Exp a) (Stencil9Ref a -> a)

data Pattern3x3 a = P3x3 [[Int]] (Stencil3x3 a -> Exp a) (Stencil3x3Ref a -> a)
data Pattern5x5 a = P5x5 [[Int]] (Stencil5x5 a -> Exp a) (Stencil5x5Ref a -> a)
data Pattern7x7 a = P7x7 [[Int]] (Stencil7x7 a -> Exp a) (Stencil7x7Ref a -> a)
data Pattern9x9 a = P9x9 [[Int]] (Stencil9x9 a -> Exp a) (Stencil9x9Ref a -> a)

data Pattern3x3x3 a = P3x3x3 [[[Int]]] (Stencil3x3x3 a -> Exp a) (Stencil3x3x3Ref a -> a)

instance Show (Pattern3 a) where show (P3 ix _ _) = show ix
instance Show (Pattern5 a) where show (P5 ix _ _) = show ix
instance Show (Pattern7 a) where show (P7 ix _ _) = show ix
instance Show (Pattern9 a) where show (P9 ix _ _) = show ix

instance Show (Pattern3x3 a) where show (P3x3 ix _ _) = show ix
instance Show (Pattern5x5 a) where show (P5x5 ix _ _) = show ix
instance Show (Pattern7x7 a) where show (P7x7 ix _ _) = show ix
instance Show (Pattern9x9 a) where show (P9x9 ix _ _) = show ix

instance Show (Pattern3x3x3 a) where show (P3x3x3 ix _ _) = show ix


pattern3 :: (P.Num a, A.Num a) => Gen (Pattern3 a)
pattern3 = do
  i <- Gen.subsequence [0..2]
  pure $
    P3 i (\(x0,x1,x2) -> P.sum (P.map ([x0,x1,x2] P.!!) i))
         (\(x0,x1,x2) -> P.sum (P.map ([x0,x1,x2] P.!!) i))

pattern5 :: (P.Num a, A.Num a) => Gen (Pattern5 a)
pattern5 = do
  i <- Gen.subsequence [0..4]
  pure $
    P5 i (\(x0,x1,x2,x3,x4) -> P.sum (P.map ([x0,x1,x2,x3,x4] P.!!) i))
         (\(x0,x1,x2,x3,x4) -> P.sum (P.map ([x0,x1,x2,x3,x4] P.!!) i))

pattern7 :: (P.Num a, A.Num a) => Gen (Pattern7 a)
pattern7 = do
  i <- Gen.subsequence [0..6]
  pure $
    P7 i (\(x0,x1,x2,x3,x4,x5,x6) -> P.sum (P.map ([x0,x1,x2,x3,x4,x5,x6] P.!!) i))
         (\(x0,x1,x2,x3,x4,x5,x6) -> P.sum (P.map ([x0,x1,x2,x3,x4,x5,x6] P.!!) i))

pattern9 :: (P.Num a, A.Num a) => Gen (Pattern9 a)
pattern9 = do
  i <- Gen.subsequence [0..8]
  pure $
    P9 i (\(x0,x1,x2,x3,x4,x5,x6,x7,x8) -> P.sum (P.map ([x0,x1,x2,x3,x4,x5,x6,x7,x8] P.!!) i))
         (\(x0,x1,x2,x3,x4,x5,x6,x7,x8) -> P.sum (P.map ([x0,x1,x2,x3,x4,x5,x6,x7,x8] P.!!) i))

pattern3x3 :: (P.Num a, A.Num a) => Gen (Pattern3x3 a)
pattern3x3 = do
  P3 i0 a0 r0 <- pattern3
  P3 i1 a1 r1 <- pattern3
  P3 i2 a2 r2 <- pattern3
  pure $
    P3x3 [i0,i1,i2]
         (\(x0,x1,x2) -> P.sum [a0 x0, a1 x1, a2 x2])
         (\(x0,x1,x2) -> P.sum [r0 x0, r1 x1, r2 x2])

pattern5x5 :: (P.Num a, A.Num a) => Gen (Pattern5x5 a)
pattern5x5 = do
  P5 i0 a0 r0 <- pattern5
  P5 i1 a1 r1 <- pattern5
  P5 i2 a2 r2 <- pattern5
  P5 i3 a3 r3 <- pattern5
  P5 i4 a4 r4 <- pattern5
  pure $
    P5x5 [i0,i1,i2,i3,i4]
         (\(x0,x1,x2,x3,x4) -> P.sum [a0 x0, a1 x1, a2 x2, a3 x3, a4 x4])
         (\(x0,x1,x2,x3,x4) -> P.sum [r0 x0, r1 x1, r2 x2, r3 x3, r4 x4])

pattern7x7 :: (P.Num a, A.Num a) => Gen (Pattern7x7 a)
pattern7x7 = do
  P7 i0 a0 r0 <- pattern7
  P7 i1 a1 r1 <- pattern7
  P7 i2 a2 r2 <- pattern7
  P7 i3 a3 r3 <- pattern7
  P7 i4 a4 r4 <- pattern7
  P7 i5 a5 r5 <- pattern7
  P7 i6 a6 r6 <- pattern7
  pure $
    P7x7 [i0,i1,i2,i3,i4,i5,i6]
         (\(x0,x1,x2,x3,x4,x5,x6) -> P.sum [a0 x0, a1 x1, a2 x2, a3 x3, a4 x4, a5 x5, a6 x6])
         (\(x0,x1,x2,x3,x4,x5,x6) -> P.sum [r0 x0, r1 x1, r2 x2, r3 x3, r4 x4, r5 x5, r6 x6])

pattern9x9 :: (P.Num a, A.Num a) => Gen (Pattern9x9 a)
pattern9x9 = do
  P9 i0 a0 r0 <- pattern9
  P9 i1 a1 r1 <- pattern9
  P9 i2 a2 r2 <- pattern9
  P9 i3 a3 r3 <- pattern9
  P9 i4 a4 r4 <- pattern9
  P9 i5 a5 r5 <- pattern9
  P9 i6 a6 r6 <- pattern9
  P9 i7 a7 r7 <- pattern9
  P9 j8 a8 r8 <- pattern9
  pure $
    P9x9 [i0,i1,i2,i3,i4,i5,i6,i7,j8]
         (\(x0,x1,x2,x3,x4,x5,x6,x7,x8) -> P.sum [a0 x0, a1 x1, a2 x2, a3 x3, a4 x4, a5 x5, a6 x6, a7 x7, a8 x8])
         (\(x0,x1,x2,x3,x4,x5,x6,x7,x8) -> P.sum [r0 x0, r1 x1, r2 x2, r3 x3, r4 x4, r5 x5, r6 x6, r7 x7, r8 x8])

pattern3x3x3 :: (P.Num a, A.Num a) => Gen (Pattern3x3x3 a)
pattern3x3x3 = do
  P3x3 i0 a0 r0 <- pattern3x3
  P3x3 i1 a1 r1 <- pattern3x3
  P3x3 i2 a2 r2 <- pattern3x3
  pure $
    P3x3x3 [i0,i1,i2]
           (\(x0,x1,x2) -> P.sum [a0 x0, a1 x1, a2 x2])
           (\(x0,x1,x2) -> P.sum [r0 x0, r1 x1, r2 x2])



stencil3Ref
    :: Elt a
    => (Stencil3Ref a -> a)
    -> SimpleBoundary a
    -> Vector a
    -> Vector a
stencil3Ref st bnd arr =
  let sh = S.shape arr
  in
  fromFunction sh
    (\ix@(Z:.n) -> let x0 = either id (arr S.!) (bound bnd sh (Z :. n-1))
                       x1 = arr S.! ix
                       x2 = either id (arr S.!) (bound bnd sh (Z :. n+1))
                   in
                   st (x0,x1,x2))

stencil5Ref
    :: Elt a
    => (Stencil5Ref a -> a)
    -> SimpleBoundary a
    -> Vector a
    -> Vector a
stencil5Ref st bnd arr =
  let sh = S.shape arr
  in
  fromFunction sh
    (\(Z:.i) ->
        let get it  = either id (arr S.!) (bound bnd sh it)
            --
            x0 = get (Z:.i-2)
            x1 = get (Z:.i-1)
            x2 = get (Z:.i)
            x3 = get (Z:.i+1)
            x4 = get (Z:.i+2)
        in
        st (x0,x1,x2,x3,x4))

stencil7Ref
    :: Elt a
    => (Stencil7Ref a -> a)
    -> SimpleBoundary a
    -> Vector a
    -> Vector a
stencil7Ref st bnd arr =
  let sh = S.shape arr
  in
  fromFunction sh
    (\(Z:.i) ->
        let get it  = either id (arr S.!) (bound bnd sh it)
            --
            x0 = get (Z:.i-3)
            x1 = get (Z:.i-2)
            x2 = get (Z:.i-1)
            x3 = get (Z:.i)
            x4 = get (Z:.i+1)
            x5 = get (Z:.i+2)
            x6 = get (Z:.i+3)
        in
        st (x0,x1,x2,x3,x4,x5,x6))

stencil9Ref
    :: Elt a
    => (Stencil9Ref a -> a)
    -> SimpleBoundary a
    -> Vector a
    -> Vector a
stencil9Ref st bnd arr =
  let sh = S.shape arr
  in
  fromFunction sh
    (\(Z:.i) ->
        let get it  = either id (arr S.!) (bound bnd sh it)
            --
            x0 = get (Z:.i-4)
            x1 = get (Z:.i-3)
            x2 = get (Z:.i-2)
            x3 = get (Z:.i-1)
            x4 = get (Z:.i)
            x5 = get (Z:.i+1)
            x6 = get (Z:.i+2)
            x7 = get (Z:.i+3)
            x8 = get (Z:.i+4)
        in
        st (x0,x1,x2,x3,x4,x5,x6,x7,x8))

stencil3x3Ref
    :: Elt a
    => (Stencil3x3Ref a -> a)
    -> SimpleBoundary a
    -> Matrix a
    -> Matrix a
stencil3x3Ref st bnd arr =
  let sh = S.shape arr
  in
  fromFunction sh
    (\(Z:.j:.i) ->
        let get it  = either id (arr S.!) (bound bnd sh it)
            --
            x0 = ( get (Z :. j-1 :. i-1), get (Z :. j-1 :. i), get (Z :. j-1 :. i+1) )
            x1 = ( get (Z :. j   :. i-1), get (Z :. j   :. i), get (Z :. j   :. i+1) )
            x2 = ( get (Z :. j+1 :. i-1), get (Z :. j+1 :. i), get (Z :. j+1 :. i+1) )
        in
        st (x0,x1,x2))

stencil5x5Ref
    :: Elt a
    => (Stencil5x5Ref a -> a)
    -> SimpleBoundary a
    -> Matrix a
    -> Matrix a
stencil5x5Ref st bnd arr =
  let sh = S.shape arr
  in
  fromFunction sh
    (\(Z:.j:.i) ->
        let get it  = either id (arr S.!) (bound bnd sh it)
            --
            x0 = ( get (Z :. j-2 :. i-2), get (Z :. j-2 :. i-1), get (Z :. j-2 :. i), get (Z :. j-2 :. i+1), get (Z :. j-2 :. i+2) )
            x1 = ( get (Z :. j-1 :. i-2), get (Z :. j-1 :. i-1), get (Z :. j-1 :. i), get (Z :. j-1 :. i+1), get (Z :. j-1 :. i+2) )
            x2 = ( get (Z :. j   :. i-2), get (Z :. j   :. i-1), get (Z :. j   :. i), get (Z :. j   :. i+1), get (Z :. j   :. i+2) )
            x3 = ( get (Z :. j+1 :. i-2), get (Z :. j+1 :. i-1), get (Z :. j+1 :. i), get (Z :. j+1 :. i+1), get (Z :. j+1 :. i+2) )
            x4 = ( get (Z :. j+2 :. i-2), get (Z :. j+2 :. i-1), get (Z :. j+2 :. i), get (Z :. j+2 :. i+1), get (Z :. j+2 :. i+2) )
        in
        st (x0,x1,x2,x3,x4))

stencil7x7Ref
    :: Elt a
    => (Stencil7x7Ref a -> a)
    -> SimpleBoundary a
    -> Matrix a
    -> Matrix a
stencil7x7Ref st bnd arr =
  let sh = S.shape arr
  in
  fromFunction sh
    (\(Z:.j:.i) ->
        let get it  = either id (arr S.!) (bound bnd sh it)
            --
            x0 = ( get (Z :. j-3 :. i-3), get (Z :. j-3 :. i-2), get (Z :. j-3 :. i-1), get (Z :. j-3 :. i), get (Z :. j-3 :. i+1), get (Z :. j-3 :. i+2), get (Z :. j-3 :. i+3) )
            x1 = ( get (Z :. j-2 :. i-3), get (Z :. j-2 :. i-2), get (Z :. j-2 :. i-1), get (Z :. j-2 :. i), get (Z :. j-2 :. i+1), get (Z :. j-2 :. i+2), get (Z :. j-2 :. i+3) )
            x2 = ( get (Z :. j-1 :. i-3), get (Z :. j-1 :. i-2), get (Z :. j-1 :. i-1), get (Z :. j-1 :. i), get (Z :. j-1 :. i+1), get (Z :. j-1 :. i+2), get (Z :. j-1 :. i+3) )
            x3 = ( get (Z :. j   :. i-3), get (Z :. j   :. i-2), get (Z :. j   :. i-1), get (Z :. j   :. i), get (Z :. j   :. i+1), get (Z :. j   :. i+2), get (Z :. j   :. i+3) )
            x4 = ( get (Z :. j+1 :. i-3), get (Z :. j+1 :. i-2), get (Z :. j+1 :. i-1), get (Z :. j+1 :. i), get (Z :. j+1 :. i+1), get (Z :. j+1 :. i+2), get (Z :. j+1 :. i+3) )
            x5 = ( get (Z :. j+2 :. i-3), get (Z :. j+2 :. i-2), get (Z :. j+2 :. i-1), get (Z :. j+2 :. i), get (Z :. j+2 :. i+1), get (Z :. j+2 :. i+2), get (Z :. j+2 :. i+3) )
            x6 = ( get (Z :. j+3 :. i-3), get (Z :. j+3 :. i-2), get (Z :. j+3 :. i-1), get (Z :. j+3 :. i), get (Z :. j+3 :. i+1), get (Z :. j+3 :. i+2), get (Z :. j+3 :. i+3) )
        in
        st (x0,x1,x2,x3,x4,x5,x6))

stencil9x9Ref
    :: Elt a
    => (Stencil9x9Ref a -> a)
    -> SimpleBoundary a
    -> Matrix a
    -> Matrix a
stencil9x9Ref st bnd arr =
  let sh = S.shape arr
  in
  fromFunction sh
    (\(Z:.j:.i) ->
        let get it  = either id (arr S.!) (bound bnd sh it)
            --
            x0 = ( get (Z :. j-4 :. i-4), get (Z :. j-4 :. i-3), get (Z :. j-4 :. i-2), get (Z :. j-4 :. i-1), get (Z :. j-4 :. i), get (Z :. j-4 :. i+1), get (Z :. j-4 :. i+2), get (Z :. j-4 :. i+3), get (Z :. j-4 :. i+4) )
            x1 = ( get (Z :. j-3 :. i-4), get (Z :. j-3 :. i-3), get (Z :. j-3 :. i-2), get (Z :. j-3 :. i-1), get (Z :. j-3 :. i), get (Z :. j-3 :. i+1), get (Z :. j-3 :. i+2), get (Z :. j-3 :. i+3), get (Z :. j-3 :. i+4) )
            x2 = ( get (Z :. j-2 :. i-4), get (Z :. j-2 :. i-3), get (Z :. j-2 :. i-2), get (Z :. j-2 :. i-1), get (Z :. j-2 :. i), get (Z :. j-2 :. i+1), get (Z :. j-2 :. i+2), get (Z :. j-2 :. i+3), get (Z :. j-2 :. i+4) )
            x3 = ( get (Z :. j-1 :. i-4), get (Z :. j-1 :. i-3), get (Z :. j-1 :. i-2), get (Z :. j-1 :. i-1), get (Z :. j-1 :. i), get (Z :. j-1 :. i+1), get (Z :. j-1 :. i+2), get (Z :. j-1 :. i+3), get (Z :. j-1 :. i+4) )
            x4 = ( get (Z :. j   :. i-4), get (Z :. j   :. i-3), get (Z :. j   :. i-2), get (Z :. j   :. i-1), get (Z :. j   :. i), get (Z :. j   :. i+1), get (Z :. j   :. i+2), get (Z :. j   :. i+3), get (Z :. j   :. i+4) )
            x5 = ( get (Z :. j+1 :. i-4), get (Z :. j+1 :. i-3), get (Z :. j+1 :. i-2), get (Z :. j+1 :. i-1), get (Z :. j+1 :. i), get (Z :. j+1 :. i+1), get (Z :. j+1 :. i+2), get (Z :. j+1 :. i+3), get (Z :. j+1 :. i+4) )
            x6 = ( get (Z :. j+2 :. i-4), get (Z :. j+2 :. i-3), get (Z :. j+2 :. i-2), get (Z :. j+2 :. i-1), get (Z :. j+2 :. i), get (Z :. j+2 :. i+1), get (Z :. j+2 :. i+2), get (Z :. j+2 :. i+3), get (Z :. j+2 :. i+4) )
            x7 = ( get (Z :. j+3 :. i-4), get (Z :. j+3 :. i-3), get (Z :. j+3 :. i-2), get (Z :. j+3 :. i-1), get (Z :. j+3 :. i), get (Z :. j+3 :. i+1), get (Z :. j+3 :. i+2), get (Z :. j+3 :. i+3), get (Z :. j+3 :. i+4) )
            x8 = ( get (Z :. j+4 :. i-4), get (Z :. j+4 :. i-3), get (Z :. j+4 :. i-2), get (Z :. j+4 :. i-1), get (Z :. j+4 :. i), get (Z :. j+4 :. i+1), get (Z :. j+4 :. i+2), get (Z :. j+4 :. i+3), get (Z :. j+4 :. i+4) )
        in
        st (x0,x1,x2,x3,x4,x5,x6,x7,x8))

stencil3x3x3Ref
    :: forall a. Elt a
    => (Stencil3x3x3Ref a -> a)
    -> SimpleBoundary a
    -> Array DIM3 a
    -> Array DIM3 a
stencil3x3x3Ref st bnd arr =
  let sh = S.shape arr
  in
  fromFunction sh
    (\(Z:.k:.j:.i) ->
        let get it  = either id (arr S.!) (bound bnd sh it)
            --
            x0 z = ( get (Z :. z :. j-1 :. i-1), get (Z :. z :. j-1 :. i), get (Z :. z :. j-1 :. i+1) )
            x1 z = ( get (Z :. z :. j   :. i-1), get (Z :. z :. j   :. i), get (Z :. z :. j   :. i+1) )
            x2 z = ( get (Z :. z :. j+1 :. i-1), get (Z :. z :. j+1 :. i), get (Z :. z :. j+1 :. i+1) )
        in
        st ((x0 (k-1), x1 (k-1), x2 (k-1))
           ,(x0 k    , x1 k,     x2 k)
           ,(x0 (k+1), x1 (k+1), x2 (k+1))))


bound :: forall sh e. Shape sh => SimpleBoundary e -> sh -> sh -> Either e sh
bound bnd sh0 ix0 =
  case go (eltR @sh) (fromElt sh0) (fromElt ix0) of
    Left e    -> Left e
    Right ix' -> Right (toElt ix')
  where
    go :: TypeR t -> t -> t -> Either e t
    go TupRunit           ()      ()      = Right ()
    go (TupRpair tsh tsz) (sh,sz) (ih,iz) = go tsh sh ih `addDim` go tsz sz iz
    go (TupRsingle t)     sh      i
      | Just Refl <- matchScalarType t (scalarType :: ScalarType Int)
      = if i P.< 0
          then case bnd of
                 Clamp      -> Right 0
                 Mirror     -> Right (-i)
                 Wrap       -> Right (sh+i)
                 Constant e -> Left e

        else if i P.>= sh
          then case bnd of
                 Clamp      -> Right (sh-1)
                 Mirror     -> Right (sh-(i-sh+2))
                 Wrap       -> Right (i-sh)
                 Constant e -> Left e
        else
          Right i
      --
      | otherwise
      = error "bound: expected shape with Int dimensions"

    Right ds `addDim` Right d = Right (ds, d)
    _        `addDim` Left e  = Left e
    Left e   `addDim` _       = Left e

