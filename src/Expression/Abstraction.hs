-- # Expression.Abstraction
-- 抽象化
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
module Expression.Abstraction
     where
-- 
-- ## import 宣言
--
import Data.Bool
import Data.Functor.Foldable
import Data.String
import Text.ParserCombinators.ReadP

import Expression.MSiki
import Expression.SSiki
import Expression.Substitution
import Expression.Variable

{- $setup
>>> :set -XOverloadedStrings
-}
--
-- ### 抽象化
--
{- |
>>> sampleMSiki
(x.(y.#x))
>>> abstract "x" sampleMSiki
(((().()).()).(().(y.#x)))
-}
abstract :: Variable -> MSiki -> MSiki
abstract x s = M4 (encodeSSiki $ occ x s)
                  (replace s x M2)
--
-- ## 抽象対（abstract pair）
--
data AbsPair
    = A1 MSiki
    | A2
    | A3 MSiki MSiki MSiki MSiki
    deriving (Eq,Ord, Show, Read)
--
-- ### 符号化と解釈
--
{- |
>>> isAbsPair (abstract "x" sampleMSiki)
True
>>> abp = abstraction "x" sampleMSiki
>>> abp
A3 (().()) () () (y.#x)
>>> instantiation abp (M1 "a")
(a.(y.#x))
-}
isAbsPair :: MSiki -> Bool
isAbsPair = \ case
    M4 a b -> case (a,b) of
        (M2,_)        -> True
        (M4 M2 M2,M2) -> True
        (M4 p q, M4 s t)
            | and [isAbsPair (M4 p s), isAbsPair (M4 q t), a /= M4 M2 M2] -> True
            | otherwise                                                   -> False
        _ -> False
    _ -> False

encodeAbsPair :: AbsPair -> MSiki
encodeAbsPair = \ case
    A1 s   -> M4 M2 s
    A2     -> M4 (M4 M2 M2) M2
    A3 p q s t -> M4 (M4 p q) (M4 s t)

decodeAbsPair :: MSiki -> AbsPair
decodeAbsPair = \ case
    M4 a b -> case (a,b) of
        (M2,t)        -> A1 t
        (M4 M2 M2,M2) -> A2
        (M4 p q, M4 s t)
            | and [isAbsPair (M4 p s), isAbsPair (M4 q t), a /= M4 M2 M2] -> A3 p q s t
            | otherwise -> error "not abstract pair"
        _ -> error "not abstract pair"
    _ -> error "not abstract pair"

abstraction :: Variable -> MSiki -> AbsPair
abstraction x = decodeAbsPair . abstract x
--
-- ### 具体化
--
instantiation :: AbsPair -> MSiki -> MSiki
instantiation a u = case a of
    A1 t -> t
    A2   -> u
    A3 p q s t -> M4 ips iqt
        where
            ips = instantiation aps u
            iqt = instantiation aqt u
            aps = decodeAbsPair (M4 p s)
            aqt = decodeAbsPair (M4 q t)
--
-- ## サンプルデータ
--
sampleMSiki :: MSiki
sampleMSiki = M4 (M1 "x") (M4 (M1 "y") (M3 (M1 "x")))
