-- # Expression.LstSpec
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
module Expression.LstSpec
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
    { describe "リスト" $ do
        { describe "Show のインスタンス" $ do
            { it "show sampleLst1 -> \"()\""
                $ show sampleLst1 `shouldBe` "()"
            ; it "show sampleLst2 -> \"(a,(),#a,(#a.()))\""
                $ show sampleLst2 `shouldBe` "(a,(),#a,(#a.()))"
            }
        ; describe "Read のインスタンス" $ do
            { it "read (show sampleLst1) == sampleLst1 --> True"
                $ read (show sampleLst1) == sampleLst1 `shouldBe` True
            ; it "read (show sampleLst2) == sampleLst2 --> True"
                $ read (show sampleLst2) == sampleLst2 `shouldBe` True
            }
        }
    }

