-- # Expression.Variable
-- 変数
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
module Expression.Variable
     where
-- 
-- ## import 宣言
--
import Data.Bool
import Data.Char
import Data.String
import Numeric.Natural
import Data.Functor.Foldable
import Text.ParserCombinators.ReadP

import Expression.Siki
{- $setup
>>> :seti -XOverloadedStrings
-}
--
-- ## Variable
-- 
newtype Variable = Variable { str_ :: String }
    deriving (Eq,Ord)

unVariable :: Variable -> String
unVariable v = v.str_

instance IsString Variable where
    fromString :: String -> Variable
    fromString = read @Variable

instance Show Variable where
    show :: Variable -> String
    show a = a.str_

instance Read Variable where
    readsPrec :: Int -> ReadS Variable
    readsPrec _ = readP_to_S rVariable

rVariable :: ReadP Variable
rVariable = Variable <$> munch1 isVarChar

isVarChar :: Char -> Bool
isVarChar c = isAscii c && isAlpha c

varChars :: [Char]
varChars = [ chr n | n <- [0 .. 127], isAlpha (chr n)]
{- |
>>> encodeVariable "A"
(●.((●.●).(●.●)))
>>> encodeChar 'A'
(●.●)
>>> encodeInt 0
(●.●)
>>> decodeInt (encodeInt 0)
0
>>> decodeChar (encodeChar 'A')
'A'
>>> encodeVariable "A"
(●.((●.●).(●.●)))
>>> encodeVariable "AB"
(●.((●.●).((●.(●.●)).(●.●))))
>>> decodeVariable (encodeVariable "AB") == "AB"
True
-}
encodeVariable :: Variable -> Siki
encodeVariable = cata phi . unVariable where
    phi = \ case
        Nil              -> E2 E1 (E2 E1 E1)
        Cons c (E2 E1 s) -> E2 E1 (E2 c' s)
            where
                c' = encodeChar c
        _                -> error "impossible"

encodeChar :: Char -> Siki
encodeChar = \ case
    c | isVarChar c
        -> encodeInt $ bool (subtract (ord 'A')) ((26 +) . subtract (ord 'a')) (isLower c) (ord c)
      | otherwise
        -> error "only ascii alphabet allowed"

encodeInt :: Int -> Siki
encodeInt = cata phi . fromIntegral @Int @Natural where
    phi :: Maybe Siki -> Siki
    phi = \ case
        Nothing -> E2 E1 E1
        Just s  -> E2 E1 s

decodeInt :: Siki -> Int
decodeInt = fromIntegral @Natural @Int . ana psi where
    psi :: Siki -> Base Natural Siki
    psi = \ case
        E2 E1 E1 -> Nothing
        E2 E1 s  -> Just s
        _        -> error "Siki is not encoded from Int"

decodeChar :: Siki -> Char
decodeChar s = case decodeInt s of
    n | 0 <= n && n < 52 -> chr $ bool (ord 'A' +) (ord 'a' +) (26 <= n) (n `mod` 26)
      | otherwise        -> error "Siki is not encoded from Char"

decodeVariable :: Siki -> Variable
decodeVariable = read @Variable . ana psi where
    psi = \ case
        E2 E1 (E2 E1 E1) -> Nil
        E2 E1 (E2 h t)   -> Cons (decodeChar h) (E2 E1 t)
        _                -> error "Siki is not encoded from Variable"
