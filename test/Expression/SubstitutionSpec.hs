-- # Expression.SubstitutionSpec
-- 代入（Substitution）のテスト
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
module Expression.SubstitutionSpec
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
import Expression.Lst
import Expression.Substitution
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
    { describe "代入（Substitution）" $ do
        { describe "subst :: MSiki -> Variable -> MSiki -> MSiki" $ do
            { it "subst sampleMSiki \"x\" (M1 \"y\") --> ((#y.()).(#y.#x))"
                $ show (subst sampleMSiki' "x" (M1 "y")) `shouldBe` "((#y.()).(#y.#x))"
            }
        ; describe "occ :: Variable -> MSiki -> SSiki" $ do
            { it "occ \"x\" sampleMSiki --> ((□.()).(□.()))"
                $ show (occ "x" sampleMSiki') `shouldBe` "((□.()).(□.()))"
            ; it "occ \"y\" sampleMSiki --> ()"
                $ show (occ "y" sampleMSiki') `shouldBe` "()"
            ; it "M1 \"x\" ∈ vLst1 sampleMSiki --> #x"
                $ show (M1 "x" ∈ vLst1 sampleMSiki') `shouldBe` "#x"
            ; it "M1 \"y\" ∈ vLst1 sampleMSiki --> ()"
                $ show (M1 "y" ∈ vLst1 sampleMSiki') `shouldBe` "()"
            } 
        }
    }

