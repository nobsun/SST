-- # Expression.AbstractionSpec
-- 抽象化のテスト
--
-- ## 言語拡張と`module`宣言
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Expression.AbstractionSpec
  ( spec
  ) where
--
-- ## `import`宣言
--
import Data.List
import Data.String
import qualified Codec.Binary.UTF8.String as U
import qualified Data.ByteString as B
import Test.Hspec
import Text.Show.Unicode

import Expression.MSiki
import Expression.Abstraction
--
-- ## 型宣言
-- 
newtype UString a = UString a deriving Eq

ustring :: B.ByteString -> UString String
ustring = UString . U.decode . B.unpack

instance IsString a => IsString (UString a) where
  fromString :: IsString a => String -> UString a
  fromString = UString . fromString
  
instance Show a => Show (UString a) where
  show :: Show a => UString a -> String
  show (UString s) = ushow s
--
-- ## `Spec`
--
spec :: Spec
spec = do 
    { describe "抽象化" $ do
        { describe "abstract :: Variable -> MSiki -> MSiki" $ do
            { it "abstract \"x\" sampleMSiki --> (((().()).()).(().(y.#x)))"
                $ show (abstract "x" sampleMSiki) `shouldBe` "(((().()).()).(().(y.#x)))"
            }
        ; describe "abstraction :: Variable -> MSiki -> AbsPair" $ do
            { it "abstraction \"x\" sampleMSiki --> A3 (().()) () () (y.#x)"
                $ show (abstraction "x" sampleMSiki) `shouldBe` "A3 (().()) () () (y.#x)"
            }
        }
    ; describe "具体化" $ do
        { describe "instantiation :: AbsPair -> MSiki -> MSiki" $ do
            { it "instantiation (abstraction \"x\" sampleMSiki) (M1 \"a\") --> (a.(y.#x))"
                $ show (instantiation (abstraction "x" sampleMSiki) (M1 "a")) `shouldBe` "(a.(y.#x))"
            }
        }
    }

