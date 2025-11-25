-- # Expression.SikiSpec
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
module Expression.SikiSpec
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
    { describe "式: sampleSiki = E2 (E2 E1 (E2 E1 E1)) (E2 (E2 E1 E1) E1)" $ do
        { describe "Show のインスタンス" $ do
            { it "sampleSiki の表示"
                $ show sampleSiki `shouldBe` "((●.(●.●)).((●.●).●))"
            }
        ; describe "Read のインスタンス" $ do
            { it "E2 (E2 E1 (E2 E1 E1)) (E2 (E2 E1 E1) E1) の読込"
                $ read "((●.(●.●)).((●.●).●))" `shouldBe` E2 (E2 E1 (E2 E1 E1)) (E2 (E2 E1 E1) E1)
            }
        ; describe "read . show = id" $ do
            { it "read (show sampleSiki) == sampleSiki"
                $ read (show sampleSiki) == sampleSiki `shouldBe` True
            }
        ; describe "式を星取表で符号化および符号化先の星取表からの復号" $ do
            { it "encodeSiki ((●.(●.●)).((●.●).●)) = ○○●○●●○○●●●"
                $ encodeSiki sampleSiki `shouldBe` [Siro,Siro,Kuro,Siro,Kuro,Kuro,Siro,Siro,Kuro,Kuro,Kuro]
            ; it "decodeSiki ○○●○●●○○●●● = ((●.(●.●)).((●.●).●))"
                $ decodeSiki [Siro,Siro,Kuro,Siro,Kuro,Kuro,Siro,Siro,Kuro,Kuro,Kuro] `shouldBe` E2 (E2 E1 (E2 E1 E1)) (E2 (E2 E1 E1) E1)
            ; it "decodeSiki . encodeSiki = id"
                $ decodeSiki (encodeSiki sampleSiki) == sampleSiki `shouldBe` True
            }
        }
    }

