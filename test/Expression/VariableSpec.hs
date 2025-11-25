-- # Expression.VariableSpec
-- 変数のテスト
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
module Expression.VariableSpec
  ( spec
  ) where

import Data.String
import qualified Codec.Binary.UTF8.String as U
import qualified Data.ByteString as B
import Test.Hspec
import Text.Show.Unicode

import Expression.Variable

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
    { describe "未定義" $ do
        { describe "未定義" $ do
            { it "未定義"
                $ "未定義" `shouldBe` "未定義"
            }
        }
    }

