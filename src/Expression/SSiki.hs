-- # Expression.SSiki
-- S式
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
module Expression.SSiki
     where
-- 
-- ## import 宣言
--
import Data.Bool
import Data.Functor.Foldable
import Data.List
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (readP_to_Prec)

import Expression.MSiki
--
-- ## S式
--
data SSiki
    = S1
    | S2 SSiki SSiki
    deriving (Eq,Ord)

data SSikiF r
    = S1F
    | S2F r r
    deriving (Eq,Ord,Functor)

type instance Base SSiki = SSikiF

instance Recursive SSiki where
    project :: SSiki -> SSikiF SSiki
    project = \ case
        S1     -> S1F
        S2 s t -> S2F s t

cataSSiki :: (SSikiF a -> a) -> SSiki -> a
cataSSiki = cata

paraSSiki :: (SSikiF (SSiki, a) -> a) -> SSiki -> a
paraSSiki = para

instance Corecursive SSiki where
    embed :: SSikiF SSiki -> SSiki
    embed = \ case
        S1F     -> S1
        S2F s t -> S2 s t

anaSSiki :: (a -> SSikiF a) -> a -> SSiki
anaSSiki = ana

apoSSiki :: (a -> SSikiF (Either SSiki a)) -> a -> SSiki
apoSSiki = apo
--
-- ### `Show` と `Read`
--
{- |
>>> sampleSSiki1
()
>>> sampleSSiki2
□
>>> sampleSSiki3
(□.())
-}
instance Show SSiki where
    showsPrec :: Int -> SSiki -> ShowS
    showsPrec _ = paraSSiki phi where
        phi = \ case
            S1F     -> showParen True id
            S2F (s,ss) (t,ts) 
                | s == S1 && t == S1 -> showChar '□'
                | otherwise          -> showParen True (ss . showChar '.' . ts)

instance Read SSiki where
    readsPrec :: Int -> ReadS SSiki
    readsPrec _ = readP_to_S rSSiki

rSSiki :: ReadP SSiki
rSSiki = rS1 +++ rS2

rS1 :: ReadP SSiki
rS1 = S1 <$ string "()"

rS2 :: ReadP SSiki
rS2 = rSqr +++ (S2 <$ char '(' <*> rSSiki <* char '.' <*> rSSiki <* char ')')

rSqr :: ReadP SSiki
rSqr = S2 S1 S1 <$ char '□'
--
-- ### 符号化と解釈
--
{- |
>>> encodeSSiki sampleSSiki1
()
>>> encodeSSiki sampleSSiki2
(().())
>>> encodeSSiki sampleSSiki3
((().()).())
>>> decodeSSiki (encodeSSiki sampleSSiki1)
()
>>> decodeSSiki (encodeSSiki sampleSSiki2)
□
>>> decodeSSiki (encodeSSiki sampleSSiki3)
(□.())
-}
encodeSSiki :: SSiki -> MSiki
encodeSSiki = cataSSiki phi where
    phi = \ case
        S1F     -> M2
        S2F s t -> M4 s t

decodeSSiki :: MSiki -> SSiki
decodeSSiki = anaSSiki psi where
    psi = \ case
        M2     -> S1F
        M4 s t -> S2F s t
        _      -> error "decodeSSiki: not encoded from SSiki"

ssqr :: SSiki      -- □
ssqr = S2 S1 S1
--
-- ## サンプルデータ
--
sampleSSiki1, sampleSSiki2, sampleSSiki3 :: SSiki
sampleSSiki1 = S1
sampleSSiki2 = ssqr
sampleSSiki3 = S2 sampleSSiki2 sampleSSiki1
