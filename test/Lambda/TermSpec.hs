-- # Lambda.TermSpec
-- 星と星取表のテスト
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
module Lambda.TermSpec
  ( spec
  ) where

import Data.String
import qualified Codec.Binary.UTF8.String as U
import qualified Data.ByteString as B
import Test.Hspec
import Text.Show.Unicode

import Lambda.Term

newtype UString a = UString a deriving Eq

ustring :: B.ByteString -> UString String
ustring = UString . U.decode . B.unpack

instance IsString a => IsString (UString a) where
  fromString :: IsString a => String -> UString a
  fromString = UString . fromString
  
instance Show a => Show (UString a) where
  show :: Show a => UString a -> String
  show (UString s) = ushow s

spec :: Spec
spec = do
    { describe "未実装" $ do
        { it "未実装"
            $ "未実装" `shouldBe` "未実装"
        }
    }
-- spec = do 
--     { describe "星（Hosi）の定義とインスタンス宣言" $ do
--         { describe "Show のインスタンス" $ do
--             { it "Kuro の表示"
--                 $ show Kuro `shouldBe` "●"
--             ; it "Siro の表示"
--                 $ show Siro `shouldBe` "○"
--             }
--         ; describe "Read のインスタンス" $ do
--             { it "Kuro の読込"
--                 $ read "●" `shouldBe` Kuro
--             ; it "Siro の読込"
--                 $ read "○" `shouldBe` Siro
--             }
--         ; describe "read . show = id" $ do
--             { it "read (show Kuro) == Kuro"
--                 $ read (show Kuro) == Kuro `shouldBe` True
--             ; it "read (show Siro) == Siro"
--                 $ read (show Siro) == Siro `shouldBe` True
--             }
--         }
--     ; describe "星取表（Hs）の定義とインスタンス宣言" $ do
--         { describe "Show のインスタンス" $ do
--             { it "sample星取表1 = [Siro,Kuro,Siro,Kuro,Kuro] の表示"
--                 $ show sample星取表1 `shouldBe` "○●○●●"
--             ; it "sample星取表2 = [Kuro,Kuro,Siro,Kuro,Kuro,Kuro,Siro,Siro,Kuro] の表示"
--                 $ show sample星取表2 `shouldBe` "●●○●●●○○●"
--             }
--         ; describe "Read のインスタンス" $ do
--             { it "○●○●● の読込"
--                 $ readHs ("○●○●●" :: String) `shouldBe` [Siro,Kuro,Siro,Kuro,Kuro]
--             ; it "●●○●●●○○● の読込"
--                 $ readHs ("●●○●●●○○●" :: String) `shouldBe` [Kuro,Kuro,Siro,Kuro,Kuro,Kuro,Siro,Siro,Kuro]
--             }
--         ; describe "read . show = id" $ do
--             { it "read (show sample星取表1) == sample星取表1"
--                 $ readHs (show sample星取表1) == sample星取表1 `shouldBe` True
--             ; it "read (show sample星取表2) == sample星取表2"
--                 $ readHs (show sample星取表2) == sample星取表2 `shouldBe` True
--             }
--         }
--     }

