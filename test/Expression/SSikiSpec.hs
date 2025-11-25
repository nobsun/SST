-- # Expression.SSikiSpec
-- 式のテスト
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
module Expression.SSikiSpec
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

import Expression.Hosi
import Expression.SSiki
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
    { describe "S式" $ do
        { describe "Show のインスタンス" $ do
            { it "sampleSSiki1 の表示"
                $ show sampleSSiki1 `shouldBe` "()"
            ; it "sampleSSiki2 の表示"
                $ show sampleSSiki2 `shouldBe` "□"
            ; it "sampleSSiki3 の表示"
                $ show sampleSSiki3 `shouldBe` "(□.())"
            }
        ; describe "Read のインスタンス" $ do
            { it "read (show sampleSSiki1) == sampleSSiki1"
                $ read (show sampleSSiki1) == sampleSSiki1 `shouldBe` True
            ; it "read (show sampleSSiki2) == sampleSSiki2"
                $ read (show sampleSSiki2) == sampleSSiki2 `shouldBe` True
            }
        ; describe "符号化と解釈" $ do
            { it "encodeSSiki sampleSSiki1 --> ()"
                $ show (encodeSSiki sampleSSiki1) `shouldBe` "()"
            ; it "encodeSSiki sampleSSiki2 --> (().())"
                $ show (encodeSSiki sampleSSiki2) `shouldBe` "(().())"
            ; it "encodeSSiki sampleSSiki3 --> ((().()).())"
                $ show (encodeSSiki sampleSSiki3) `shouldBe` "((().()).())"
            ; it "decodeSSiki (encodeSSiki sampleSSiki1) == sampleSSiki1"
                $ decodeSSiki (encodeSSiki sampleSSiki1) == sampleSSiki1 `shouldBe` True
            ; it "decodeSSiki (encodeSSiki sampleSSiki2) == sampleSSiki2"
                $ decodeSSiki (encodeSSiki sampleSSiki2) == sampleSSiki2 `shouldBe` True
            ; it "decodeSSiki (encodeSSiki sampleSSiki3) == sampleSSiki3"
                $ decodeSSiki (encodeSSiki sampleSSiki3) == sampleSSiki3 `shouldBe` True
            }
        }
    }

