-- # Expression.Hosi
-- 星と星取表
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
module Expression.Hosi
     where
-- 
-- ## import 宣言
--
import Data.List
import Text.ParserCombinators.ReadP
--
-- ## 星 `Hosi`
--
data Hosi
    = Kuro
    | Siro
    deriving (Eq, Ord, Enum, Bounded)

{- |
>>> sample黒星
●
>>> sample白星
○
-}
instance Show Hosi where
    showsPrec :: Int -> Hosi -> ShowS
    showsPrec _ = \ case
        Kuro -> ('●' :)
        Siro -> ('○' :)

{- |
>>> read @Hosi (show sample黒星)
●
>>> read @Hosi (show sample白星)
○
>>> read @Hosi (show sample黒星) == Kuro
True
>>> read @Hosi (show sample白星) == Siro
True
-}
instance Read Hosi where
    readsPrec :: Int -> ReadS Hosi
    readsPrec _ = \ case
        '●':rs -> [(Kuro, rs)]
        '○':rs -> [(Siro, rs)]
        _      -> []
--
-- ## 星取表 `Hs`
--
type Hs = [Hosi]

{- |
>>> sample星取表1
○●○●●
>>> sample星取表2
●●○●●●○○●
>>> read (show sample星取表1) :: Hs
○●○●●
>>> read (show sample星取表2) :: Hs
●●○●●●○○●
>>> read (show sample星取表1) == sample星取表1
True
>>> read (show sample星取表2) == sample星取表2
True
-}
instance {-# OVERLAPPING #-} Show Hs where
    showsPrec :: Int -> Hs -> ShowS
    showsPrec _ = \ case
        []   -> id
        h:hs -> shows h . shows hs

instance {-# OVERLAPPING #-} Read Hs where
    readsPrec :: Int -> ReadS Hs
    readsPrec _ = readP_to_S rHs

rHs :: ReadP Hs
rHs = map (read . singleton) 
  <$> munch (`elem` concatMap show hosis)

hosis :: [Hosi]
hosis = [minBound .. maxBound]

showHs :: Hs -> String
showHs = show

readHs :: String -> Hs
readHs = read

{- sample data -}

sample黒星 :: Hosi
sample黒星 = Kuro
sample白星 :: Hosi
sample白星 = Siro
sample星取表1 :: Hs
sample星取表1 = [Siro, Kuro, Siro, Kuro, Kuro]
sample星取表2 :: Hs
sample星取表2 = (toEnum . fromEnum . even @Int)
            <$> [3,1,4,1,5,9,2,6,5]

