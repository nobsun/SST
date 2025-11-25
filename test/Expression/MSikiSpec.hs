-- # Expression.MSikiSpec
-- M式のテスト
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
module Expression.MSikiSpec
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
import Expression.Siki
import Expression.MSiki
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
    { describe (intercalate "\n"
                     ["M式"
                    ,"  sampleMSiki1 = M1 \"a\""
                    ,"  sampleMSiki2 = M2"
                    ,"  sampleMSiki3 = M3 sampleMSiki1"
                    ,"  sampleMSiki4 = M4 sampleMSiki3 sampleMSiki2"
                    ]) $ do
        { describe "Show のインスタンス" $ do
            { it "sampleMSiki1 の表示"
                $ show sampleMSiki1 `shouldBe` "a"
            ; it "sampleMSiki2 の表示"
                $ show sampleMSiki2 `shouldBe` "()"
            ; it "sampleMSiki3 の表示"
                $ show sampleMSiki3 `shouldBe` "#a"
            ; it "sampleMSiki4 の表示"
                $ show sampleMSiki4 `shouldBe` "(#a.())"
            }
        ; describe "Read のインスタンス" $ do
            { it "read (show sampleMSiki4) == sampleMSiki4"
                $ read (show sampleMSiki4) == sampleMSiki4 `shouldBe` True
            }
        ; describe "M式を式で符号化および符号化先の式からの復号" $ do
            { it "decodeMSiki . encodeSiki = id"
                $ decodeMSiki (encodeMSiki sampleMSiki4') == sampleMSiki4' `shouldBe` True
            }
        ; describe "M式の大きさ" $ do
            { it "sizeMSiki sampleMSiki1 --> 0"
                $ sizeMSiki sampleMSiki1 `shouldBe` 0
            ; it "sizeMSiki sampleMSiki2 --> 0"
                $ sizeMSiki sampleMSiki2 `shouldBe` 0
            ; it "sizeMSiki sampleMSiki3 --> 1"
                $ sizeMSiki sampleMSiki3 `shouldBe` 1
            ; it "sizeMSiki sampleMSiki4 --> 2"
                $ sizeMSiki sampleMSiki4 `shouldBe` 2
            ; it "sizeMSiki (M3 sampleMSiki4) --> 3"
                $ sizeMSiki (M3 sampleMSiki4) `shouldBe` 3
            ; it "sizeMSiki (M4 sampleMSiki4 sampleMSiki3) --> 4"
                $ sizeMSiki (M4 sampleMSiki4 sampleMSiki3) `shouldBe` 4
            }
        }
    }
