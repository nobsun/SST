-- # Expression.Substitution
-- 代入（Substitution）
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
module Expression.Substitution
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
import Expression.Lst
import Expression.SSiki
import Expression.Variable
{- $setup
>>> :seti -XOverloadedStrings
-}
--
-- ### 置き換え（replace）
-- 
replace :: MSiki -> Variable -> MSiki -> MSiki
replace s x t = paraMSiki phi s where
    phi = \ case
        M1F v
            | x == v    -> t
            | otherwise -> M1 v
        M2F             -> M2
        M3F (s',_)      -> M3 s'
        M4F (_,a) (_,b) -> M4 a b
--
-- ### 置換（substitution）
--
{- |
>>> sampleMSiki'
((x.()).(x.#x))
>>> subst sampleMSiki' "x" (M1 "y")
((#y.()).(#y.#x))
-}
subst :: MSiki -> Variable -> MSiki -> MSiki
subst s x t = replace s x (M3 t)

substitute :: MSiki -> [(Variable, MSiki)] -> MSiki
substitute = foldl' phi where
    phi s (x, t) = subst s x t
--
-- ### 出現位置
--
{- |
>>> sampleMSiki'
((x.()).(x.#x))
>>> occ "x" sampleMSiki'
((□.()).(□.()))
>>> M1 "x" ∈ vLst1 sampleMSiki'
#x
>>> occ "y" sampleMSiki'
()
>>> M1 "y" ∈ vLst1 sampleMSiki'
()
-}

occ :: Variable -> MSiki ->  SSiki
occ x = cataMSiki phi where
    phi = \ case
        M1F v
            | x == v    -> ssqr
            | otherwise -> S1
        M2F             -> S1
        M3F _           -> S1
        M4F s t         -> case (s, t) of
            (S1,S1)     -> S1
            _           -> S2 s t
--
-- ## サンプルデータ
--
sampleMSiki' :: MSiki
sampleMSiki' = M4 (M4 (M1 "x") M2) (M4 (M1 "x") (M3 (M1 "x")))
