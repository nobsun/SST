-- # Expression.MSiki
-- M式
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
module Expression.MSiki
     where
-- 
-- ## import 宣言
--
import Data.Bool
import Data.Functor.Foldable
import Data.List
import Text.ParserCombinators.ReadP

import Expression.Hosi
import Expression.Siki
import Expression.Variable
{- $setup
>>> :seti -XOverloadedStrings
-}
--
-- ## M式
--
data MSiki
    = M1 Variable
    | M2
    | M3 MSiki
    | M4 MSiki MSiki
    deriving (Eq, Ord)

isVar :: MSiki -> Bool
isVar = \ case
    M1 _ -> True
    _    -> False

isNull :: MSiki -> Bool
isNull = \ case
    M2 -> True
    _  -> False

isQuote :: MSiki -> Bool
isQuote = \ case
    M3 _ -> True
    _    -> False

data MSikiF r
    = M1F Variable
    | M2F
    | M3F r
    | M4F r r
    deriving (Eq, Ord, Functor)

type instance Base MSiki = MSikiF

instance Recursive MSiki where
    project :: MSiki -> MSikiF MSiki
    project = \ case
        M1 v   -> M1F v
        M2     -> M2F
        M3 t   -> M3F t
        M4 s t -> M4F s t

cataMSiki :: (MSikiF a -> a) -> MSiki -> a
cataMSiki = cata

paraMSiki :: (MSikiF (MSiki, a) -> a) -> MSiki -> a
paraMSiki = para

instance Corecursive MSiki where
    embed :: MSikiF MSiki -> MSiki
    embed = \ case
        M1F v   -> M1 v
        M2F     -> M2
        M3F t   -> M3 t
        M4F s t -> M4 s t

anaMSiki :: (a -> MSikiF a) -> a -> MSiki
anaMSiki = ana

apoMSiki :: (a -> MSikiF (Either MSiki a)) -> a -> MSiki
apoMSiki = apo
--
-- ### `Show`と`Read`
--
{- |
>>> sampleMSiki1
a
>>> sampleMSiki2
()
>>> sampleMSiki3
#a
>>> sampleMSiki4
(#a.())
>>> read @MSiki $ show sampleMSiki1
a
>>> read @MSiki $ show sampleMSiki2
()
>>> read @MSiki $ show sampleMSiki3
#a
>>> read @MSiki $ show sampleMSiki4
(#a.())
-}
instance Show MSiki where
    showsPrec :: Int -> MSiki -> ShowS
    showsPrec _ = cataMSiki phi where
        phi = \ case
            M1F v   -> shows v
            M2F     -> ('(' :) . (')' :)
            M3F s   -> ('#' :) . s
            M4F s t -> showParen True (s . ('.' :) . t)

instance Read MSiki where
    readsPrec :: Int -> ReadS MSiki
    readsPrec _ = readP_to_S rMSiki

rMSiki :: ReadP MSiki
rMSiki = rM1 +++ rM2 +++ rM3 +++ rM4

rM1 :: ReadP MSiki
rM1 = M1 <$> rVariable

rM2 :: ReadP MSiki
rM2 = M2 <$ string "()"

rM3 :: ReadP MSiki
rM3 = M3 <$ char '#' <*> rMSiki

rM4 :: ReadP MSiki
rM4 = uncurry M4 
  <$> between (char '(') (char ')')
              ((,) <$> rMSiki <* char '.' <*> rMSiki)

--
-- ### 符号化と解釈
--
{- |
>>> sampleMSiki4
(#a.())
>>> print $ encodeMSiki sampleMSiki4
(((●.((●.(●.(●.(●.(●.(●.(●.(●.(●.(●.(●.(●.(●.(●.(●.(●.(●.(●.(●.(●.(●.(●.(●.(●.(●.(●.(●.●))))))))))))))))))))))))))).(●.●))).●).(●.●))
>>> print $ decodeMSiki (encodeMSiki sampleMSiki4)
(#a.())
-}
encodeMSiki :: MSiki -> Siki
encodeMSiki = cataMSiki phi where
    phi :: MSikiF Siki -> Siki
    phi = \ case
        M1F v   -> encodeVariable v
        M2F     -> E2 E1 E1
        M3F t   -> E2 t E1
        M4F s t -> E2 s t

decodeMSiki :: Siki -> MSiki
decodeMSiki = anaMSiki phi where
    phi = \ case
        E2 s t -> case s of
            E1     -> case t of
                E1     -> M2F
                _      -> M1F (decodeVariable (E2 s t))
            E2 _ _ -> case t of
                E1     -> M3F s
                E2 _ _ -> M4F s t
        E1     -> error "decodeMSiki: M式を符号化して構成した式ではない"
--
-- ### M式の大きさ
--
{- |
>>> sizeMSiki sampleMSiki1
0
>>> sizeMSiki sampleMSiki2
0
>>> sizeMSiki sampleMSiki3
1
>>> sizeMSiki sampleMSiki4
2
>>> sizeMSiki (M3 sampleMSiki4)
3
>>> sizeMSiki (M4 sampleMSiki4 sampleMSiki3)
4
-}
sizeMSiki :: MSiki -> Int
sizeMSiki = cataMSiki phi where
    phi = \ case
        M1F _   -> 0
        M2F     -> 0
        M3F s   -> succ s
        M4F s t -> succ (s + t)
--
-- サンプルデータ
--
sampleMSiki1 :: MSiki
sampleMSiki1 = M1 "a"

sampleMSiki2 :: MSiki
sampleMSiki2 = M2

sampleMSiki3 :: MSiki
sampleMSiki3 = M3 sampleMSiki1

sampleMSiki3' :: MSiki
sampleMSiki3' = M3 sampleMSiki2

sampleMSiki4 :: MSiki
sampleMSiki4 = M4 sampleMSiki3 sampleMSiki2

sampleMSiki4' :: MSiki
sampleMSiki4' = M4 sampleMSiki3' sampleMSiki2
