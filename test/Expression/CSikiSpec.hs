-- # Expression.CSikiSpec
-- C式のテスト
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
module Expression.CSikiSpec
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
import Expression.CSiki
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
noTest :: Bool
noTest = () /= ()

spec :: Spec
spec = do 
    { describe "C式" $ do
        { describe "isCSiki :: MSiki -> Bool" $ do
            { it "isCSiki sampleMSiki1 --> False"
                $ isCSiki sampleMSiki1 `shouldBe` False
            ; it "isCSiki sampleMSiki2 --> True"
                $ isCSiki sampleMSiki2 `shouldBe` True
            ; it "isCSiki sampleMSiki3 --> False"
                $ isCSiki sampleMSiki3 `shouldBe` False
            ; it "isCSiki sampleMSiki3' --> True"
                $ isCSiki sampleMSiki3' `shouldBe` True
            ; it "isCSiki sampleMSiki4 --> False"
                $ isCSiki sampleMSiki4 `shouldBe` False
            ; it "isCSiki sampleMSiki4' --> True"
                $ isCSiki sampleMSiki4' `shouldBe` True            
            }
        ; describe "encodeCSiki :: CSiki -> MSiki" $ do
            { it "encodeCSiki sampleCSiki1 --> ()"
                $ show (encodeCSiki sampleCSiki1) `shouldBe` "()"
            ; it "encodeCSiki sampleCSiki2 --> #()"
                $ show (encodeCSiki sampleCSiki2) `shouldBe` "#()"
            ; it "encodeCSiki sampleCSiki3 --> (#().())"
                $ show (encodeCSiki sampleCSiki3) `shouldBe` "(#().())"
            }
        ; describe "decodeCSiki :: MSiki -> CSiki" $ do
            { it "decodeCSiki (encodeCSiki sampleCSiki1) == sampleCSiki1 --> True"
                $ decodeCSiki (encodeCSiki sampleCSiki1) == sampleCSiki1 `shouldBe` True
            ; it "decodeCSiki (encodeCSiki sampleCSiki2) == sampleCSiki2 --> True"
                $ decodeCSiki (encodeCSiki sampleCSiki2) == sampleCSiki2 `shouldBe` True
            ; it "decodeCSiki (encodeCSiki sampleCSiki3) == sampleCSiki3 --> True"
                $ decodeCSiki (encodeCSiki sampleCSiki3) == sampleCSiki3 `shouldBe` True
            }
        }
    }
