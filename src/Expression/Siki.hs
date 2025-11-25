-- # Expression.Siki
-- 式
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
module Expression.Siki
     where
-- 
-- ## import 宣言
--
import Data.Bool
import Data.Functor.Foldable
import Data.List
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (readP_to_Prec)

import Expression.Hosi
--
-- ## 式
--
data Siki
    = E1
    | E2 Siki Siki
    deriving (Eq, Ord)

isE1 :: Siki -> Bool
isE1 = \ case
    E1 -> True
    _  -> False

isE2 :: Siki -> Bool
isE2 = not . isE1

data SikiF r
    = E1F
    | E2F r r
    deriving (Eq, Ord, Functor)

type instance Base Siki = SikiF

instance Recursive Siki where
    project :: Siki -> SikiF Siki
    project = \ case
        E1     -> E1F
        E2 s t -> E2F s t

cataSiki :: (SikiF a -> a) -> Siki -> a
cataSiki = cata

paraSiki :: (SikiF (Siki, a) -> a) -> Siki -> a
paraSiki = para

instance Corecursive Siki where
    embed :: SikiF Siki -> Siki
    embed = \ case
        E1F     -> E1
        E2F s t -> E2 s t

anaSiki :: (a -> SikiF a) -> a -> Siki
anaSiki = ana

apoSiki :: (a -> SikiF (Either Siki a)) -> a -> Siki
apoSiki = apo
--
-- ### `Show` と `Read`
--
{- |
>>> sampleSiki
((●.(●.●)).((●.●).●))
>>> read @Siki (show @Siki sampleSiki)
((●.(●.●)).((●.●).●))
-}
instance Show Siki where
    showsPrec :: Int -> Siki -> ShowS
    showsPrec _ = cataSiki phi where
        phi = \ case
            E1F       -> ('●' :)
            E2F ss ts -> showParen True (ss . ('.' :) . ts)

instance Read Siki where
    readsPrec :: Int -> ReadS Siki
    readsPrec _ = readP_to_S rSiki

rSiki :: ReadP Siki
rSiki = rE1 +++ rE2

rE1 :: ReadP Siki
rE1 = E1 <$ char '●'

rE2 :: ReadP Siki
rE2 = between (char '(') (char ')')
              (E2 <$> rSiki <* char ('.') <*> rSiki)
--
-- ### 符号化
--
{- |
>>> encodeSiki sampleSiki
○○●○●●○○●●●
-}
encodeSiki :: Siki -> Hs
encodeSiki = cataSiki phi where
    phi = \ case
        E1F     -> [Kuro]
        E2F s t -> [Siro] ++ s ++ t
{- |
>>> sampleHs
○○●○●●○○●●●
>>> isSikiHs sampleHs
True
-}
isSikiHs :: Hs -> Bool
isSikiHs hs = case hs of
    [Kuro]    -> True
    Siro : rs -> foldr phi False (zip (inits rs) (tails rs)) where
        phi (s,t) b = bool (not (isSikiHs t) && b) (isSikiHs t) (isSikiHs s)
    _         -> False
{- |
>>> decodeSiki sampleHs
((●.(●.●)).((●.●).●))
-}
decodeSiki :: Hs -> Siki
decodeSiki = anaSiki psi where
    psi = \ case
        [Kuro]    -> E1F
        Siro : rs -> case break (isSikiHs . fst) (zip (inits rs) (tails rs)) of
            (_, (ps,qs):_) -> E2F ps qs
            _              -> error "decodeSiki: 式を符号化して構成した星取表ではない"
        _         -> error "decodeSiki: 式を符号化して構成した星取表ではない"
--
-- サンプルデータ
--
sampleHs :: Hs
sampleHs = [Siro,Siro,Kuro,Siro,Kuro,Kuro,Siro,Siro,Kuro,Kuro,Kuro]

sampleSiki :: Siki
sampleSiki  = E2 (E2 E1 (E2 E1 E1)) (E2 (E2 E1 E1) E1)
