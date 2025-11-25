-- # Expression.Lst
-- リスト
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
module Expression.Lst
     where
-- 
-- ## import 宣言
--
import Data.Bool
import Data.Functor.Foldable
import Data.List
import Text.ParserCombinators.ReadP

import Expression.MSiki
{- $setup
>>> :set -XOverloadedStrings
-}
--
-- ## リスト
--
data Lst
    = Nl
    | Cns MSiki Lst
    deriving (Eq,Ord)

data LstF r
    = NlF
    | CnsF MSiki r
    deriving (Eq,Ord,Functor)

type instance Base Lst = LstF

instance Recursive Lst where
    project :: Lst -> LstF Lst
    project = \ case
        Nl      -> NlF
        Cns h t -> CnsF h t

cataLst :: (LstF a -> a) -> Lst -> a
cataLst = cata

paraLst :: (LstF (Lst, a) -> a) -> Lst -> a
paraLst = para

instance Corecursive Lst where
    embed :: LstF Lst -> Lst
    embed = \ case
        NlF      -> Nl
        CnsF h t -> Cns h t

anaLst :: (a -> LstF a) -> a -> Lst
anaLst = ana

apoLst :: (a -> LstF (Either Lst a)) -> a -> Lst
apoLst = apo
--
-- ### `Show` と `Read`
--
{- |
>>> sampleLst1
()
>>> sampleLst2
(a,(),#a,(#a.()))
-}
instance Show Lst where
    showsPrec :: Int -> Lst -> ShowS
    showsPrec i = paraLst phi where
        phi = \ case
            NlF          -> showParen (i == 0) id
            CnsF h (t,_) -> case t of
                Nl -> showParen (i == 0) (shows h)
                _  -> showParen (i == 0) (shows h . showChar ',' . showsPrec 1 t)
{- |
>>> read @Lst "(a,(),#a,(#a.()))"
(a,(),#a,(#a.()))
>>> read @Lst (show sampleLst2) == sampleLst2
True
-}
instance Read Lst where
    readsPrec :: Int -> ReadS Lst
    readsPrec _ = readP_to_S rLst

rLst :: ReadP Lst
rLst = rNl +++ rCns

rNl :: ReadP Lst
rNl = Nl <$ char '(' <* char ')'

rCns :: ReadP Lst
rCns = foldr Cns Nl <$> between (char '(')  (char ')')
                                (sepBy1 rMSiki (char ','))
--
-- ### 符号化と解釈
--
{- |
>>> isLst sampleMSiki1
False
>>> isLst sampleMSiki2
True
>>> isLst sampleMSiki3
False
>>> isLst sampleMSiki4
True
-}
isLst :: MSiki -> Bool
isLst = cataMSiki phi where
    phi = \ case
        M1F _   -> False
        M2F     -> True
        M3F _   -> False
        M4F _ t -> t
{- |
>>> encodeLst sampleLst1
()
>>> encodeLst sampleLst2
(a.(().(#a.((#a.()).()))))
>>> decodeLst (encodeLst sampleLst1) == sampleLst1
True
>>> decodeLst (encodeLst sampleLst2) == sampleLst2
True
-}
encodeLst :: Lst -> MSiki
encodeLst = cata phi where
    phi = \ case
        NlF      -> M2
        CnsF h t -> M4 h t

decodeLst :: MSiki -> Lst
decodeLst = ana psi where
    psi = \ case
        M2     -> NlF
        M4 s t -> CnsF s t
        _      -> error "decodeLst: リストを符号化して構成したM式ではない"
--
-- ### リストの要素
--
{- |
>>> sampleMSiki1 ∈ sampleLst2
#a
>>> M1 "x" ∈ sampleLst2
()
-}
(∈) :: MSiki -> Lst -> MSiki
s ∈ l = cataLst phi l where
    phi = \ case 
        NlF -> M2
        CnsF h t
            | h == s    -> M3 s
            | otherwise -> t
--
-- ### リストの長さ
--
{- |
>>> lengthLst sampleLst1
0
>>> lengthLst sampleLst2
4
-}
lengthLst :: Lst -> Int
lengthLst = cataLst phi where
    phi = \ case
        NlF      -> 0
        CnsF _ t -> succ t
--
-- ### リストの和
-- 
{- |
>>> sampleLst1 ⊕ sampleLst2
(a,(),#a,(#a.()))
>>> sampleLst2 ⊕ sampleLst1
(a,(),#a,(#a.()))
>>> sampleLst1 ⊕ sampleLst2 == sampleLst2 ⊕ sampleLst1
True
>>> sampleLst2 ⊕ sampleLst2
(a,(),#a,(#a.()),a,(),#a,(#a.()))
-}
(⊕) :: Lst -> Lst -> Lst
l ⊕ m = cataLst phi l where
    phi = \ case
        NlF      -> m
        CnsF h t -> Cns h t
--
-- ### リストの差
--
{- |
>>> sampleLst2 ⊖ sampleMSiki1
((),#a,(#a.()))
>>> sampleLst2 ⊖ sampleMSiki2
(a,#a,(#a.()))
>>> sampleLst2 ⊖ sampleMSiki3
(a,(),(#a.()))
>>> sampleLst2 ⊖ sampleMSiki4
(a,(),#a)
>>> (sampleLst2 ⊕ sampleLst2) ⊖ sampleMSiki4
(a,(),#a,a,(),#a)
-}
(⊖) :: Lst -> MSiki -> Lst
l ⊖ m = cataLst phi l where
    phi = \ case
        NlF -> Nl
        CnsF h t
            | h == m    -> t
            | otherwise -> Cns h t
--
-- ## 変数リスト
--
{- |
>>> vLst (encodeLst sampleLst2)
(a,a,a)
>>> vLst1 (encodeLst sampleLst2)
(a)
>>> vLst2 (encodeLst sampleLst2)
(a,a)
-}
vLst :: MSiki -> Lst
vLst = cataMSiki phi where
    phi = \ case
        M1F v   -> Cns (M1 v) Nl
        M2F     -> Nl
        M3F s   -> s
        M4F s t -> s ⊕ t

vLst1 :: MSiki -> Lst
vLst1 = cataMSiki phi where
    phi = \ case
        M1F v   -> Cns (M1 v) Nl
        M2F     -> Nl
        M3F _   -> Nl
        M4F s t -> s ⊕ t

vLst2 :: MSiki -> Lst
vLst2 = paraMSiki phi where
    phi = \ case
        M1F _           -> Nl
        M2F             -> Nl
        M3F (s,_)       -> vLst s
        M4F (_,s) (_,t) -> s ⊕ t
--
-- ## サンプルデータ
--
sampleLst1 :: Lst
sampleLst1 = Nl

sampleLst2 :: Lst
sampleLst2 = Cns sampleMSiki1 (Cns sampleMSiki2 (Cns sampleMSiki3 (Cns sampleMSiki4 Nl)))
