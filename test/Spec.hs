{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import           Hedgehog
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Gen.QuickCheck   as Gen
import qualified Hedgehog.Range            as Range
import           Hedgehog.Function
import           Test.QuickCheck.Arbitrary (Arbitrary)
import           Control.Monad
import           Control.Applicative
import           Data.Function

import Lib

main :: IO ()
main = do
    void $ do
        checkParallel $ Group "identity" [("identity", iso identity)]
        checkParallel
            $ Group "addition"
                    [ ("addition", iso $ addition)
                    , ("associativity", iso $ additionAssoc @Int @Int @Int)
                    , ("commutativity", iso $ additionCommut @Int @Int)
                    ]
        checkParallel
            $ Group "multiplication"
                    [ ("multiplication", iso $ multiplication @Int)
                    , ("distributivity", iso $ multiplicativeDistribution @Int @Int @Int)
                    , ("associativity", iso $ multiplicationAssoc @Int @Int @Int)
                    , ("commutativity", iso $ multiplicationCommut @Int @Int)
                    ]
        checkParallel
            $ Group "exponentiation"
                    [ ("zero rule", iso $ zeroRule @Int)
                    , ("a^1", iso $ aToTheOne @Int)
                    , ("a^2", iso $ aSquared @Int)
                    , ("2^2", iso $ twoSquared)
                    -- , ("distribution", eqWith fnEq (_) $ exponentialDistribution @Int @Int @Int)
                    , ("commutativity", iso $ multiplicationCommut @Int @Int)
                    ]

func :: (Show a, Show b, Arg a, Vary a, Arbitrary b) => PropertyT IO (a -> b)
func = forAllFn (fn Gen.arbitrary)

eqWith :: (a -> a -> PropertyT IO Bool) -> (PropertyT IO a) -> Equal a b -> Property
eqWith eq g (to, from) = property $ do
    a <- g
    eq (from $ to a) a >>= assert

-- fnEq :: (Show a, Arbitrary a, Eq b) => (a -> b) -> (a -> b) -> PropertyT IO Bool
-- fnEq f g = do
--     a <- forAll Gen.arbitrary
--     pure $ ((==) `on` ($ a)) f g

iso :: forall a b. (Eq a, Show a, Arbitrary a) => Equal a b -> Property
iso eq = eqWith (\a b -> pure $ a == b) (forAll Gen.arbitrary) eq
