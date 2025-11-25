-- # Expression.CSiki
-- C式
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
module Expression.CSiki
     where
-- 
-- ## import 宣言
--
import Data.Bool
import Data.Functor.Foldable
import Data.List
import Text.ParserCombinators.ReadP

import Expression.MSiki
--
-- ## C式
--
data CSiki
    = C1
    | C2 CSiki
    | C3 CSiki CSiki
    deriving (Eq, Ord)

data CSikiF r
    = C1F
    | C2F r
    | C3F r r
    deriving (Eq, Ord, Functor)

type instance Base CSiki = CSikiF

instance Recursive CSiki where
    project :: CSiki -> CSikiF CSiki
    project = \ case
        C1     -> C1F
        C2 s   -> C2F s
        C3 s t -> C3F s t

cataCSiki :: (CSikiF a -> a) -> CSiki -> a
cataCSiki = cata

paraCSiki :: (CSikiF (CSiki, a) -> a) -> CSiki -> a
paraCSiki = para

instance Corecursive CSiki where
    embed :: CSikiF CSiki -> CSiki
    embed = \ case
        C1F     -> C1
        C2F s   -> C2 s
        C3F s t -> C3 s t

anaCSiki :: (a -> CSikiF a) -> a -> CSiki
anaCSiki = ana

apoCSiki :: (a -> CSikiF (Either CSiki a)) -> a -> CSiki
apoCSiki = apo
--
-- ## 符号化と解釈
--
{- |
>>> isCSiki sampleMSiki1
False
>>> isCSiki sampleMSiki2
True
>>> isCSiki sampleMSiki3
False
>>> isCSiki sampleMSiki3'
True
>>> isCSiki sampleMSiki4
False
>>> isCSiki sampleMSiki4'
True
>>> encodeCSiki sampleCSiki1
()
>>> encodeCSiki sampleCSiki2
#()
>>> encodeCSiki sampleCSiki3
(#().())
-}
isCSiki :: MSiki -> Bool
isCSiki = cataMSiki phi where
    phi = \ case
        M1F _   -> False
        M2F     -> True
        M3F s   -> s
        M4F s t -> s && t

encodeCSiki :: CSiki -> MSiki
encodeCSiki = cataCSiki phi where
    phi = \ case
        C1F     -> M2
        C2F s   -> M3 s
        C3F s t -> M4 s t

decodeCSiki :: MSiki -> CSiki
decodeCSiki = cataMSiki phi where
    phi = \ case
        M2F     -> C1
        M3F s   -> C2 s
        M4F s t -> C3 s t
        _       -> error "not encoded from CSiki"
--
-- ## サンプルデータ
-- 
sampleCSiki1, sampleCSiki2, sampleCSiki3
    :: CSiki
sampleCSiki1 = C1
sampleCSiki2 = C2 sampleCSiki1
sampleCSiki3 = C3 sampleCSiki2 sampleCSiki1
