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
import Expression.Hosi
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (readP_to_Prec)
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

>>> decodeSiki [Kuro,Siro]
decodeSiki: 式を符号化して構成した星取表ではない
>>> decodeSiki [Siro,Kuro,Siro]
decodeSiki: 式を符号化して構成した星取表ではない
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
-- ### 変数
--
type Variable = (Siki, Siki)

instance {-# OVERLAPPING #-} Show Variable where
    showsPrec :: Int -> Variable -> ShowS
    showsPrec _ = \ case
        (s,t) -> shows (E2 s t)

instance {-# OVERLAPPING #-} Read Variable where
    readsPrec :: Int -> ReadS Variable
    readsPrec _ = readP_to_S rVariable

rVariable :: ReadP Variable
rVariable = phi <$> rE2 where
    phi = \ case
        E2 s t -> (s,t)
        _      -> error "not variable"
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
V(●.(●.●))

>>> sampleMSiki2
()

>>> sampleMSiki3
#V(●.(●.●))

>>> sampleMSiki4
(#V(●.(●.●)).())

>>> read @MSiki $ show sampleMSiki1
V(●.(●.●))

>>> read @MSiki $ show sampleMSiki2
()

>>> read @MSiki $ show sampleMSiki3
#V(●.(●.●))

>>> read @MSiki $ show sampleMSiki4
(#V(●.(●.●)).())
-}
instance Show MSiki where
    showsPrec :: Int -> MSiki -> ShowS
    showsPrec _ = paraMSiki phi where
        phi = \ case
            M1F v      -> ('V' :) . shows v
            M2F        -> ('(' :) . (')' :)
            M3F (_,ts) -> ('#' :) . ts
            M4F (s,ss) (t,ts)
                | s == M2 && t == M2 -> ('□' :) 
                | otherwise          -> showParen True (ss . ('.' :) . ts)

instance Read MSiki where
    readsPrec :: Int -> ReadS MSiki
    readsPrec _ = readP_to_S rMSiki

rMSiki :: ReadP MSiki
rMSiki = rM1 +++ rM2 +++ rM3 +++ rM4

rM1 :: ReadP MSiki
rM1 = phi <$ char 'V' <*> rE2 where
    phi = \ case
        E2 s t -> M1 (s,t)
        _      -> error "rM1: invalid syntax"

rM2 :: ReadP MSiki
rM2 = M2 <$ string "()"

rM3 :: ReadP MSiki
rM3 = M3 <$ char '#' <*> rMSiki

rM4 :: ReadP MSiki
rM4 = rMDottedPair
  +++ (uncurry M4 
  <$> between (char '(') (char ')')
              ((,) <$> rMSiki <* char '.' <*> rMSiki))

rMDottedPair :: ReadP MSiki
rMDottedPair = M4 M2 M2 <$ char '□'
--
-- ### 符号化と解釈
--
{- |
>>> encodeMSiki sampleMSiki4
(((●.(●.(●.●))).●).(●.●))

>>> decodeMSiki (E2 (E2 (E2 E1 (E2 E1 (E2 E1 E1))) E1) (E2 E1 E1))
(#V(●.(●.●)).())
-}
encodeMSiki :: MSiki -> Siki
encodeMSiki = cataMSiki phi where
    phi = \ case
        M1F (a,b) -> E2 E1 (E2 a b)
        M2F       -> E2 E1 E1
        M3F t     -> E2 t E1
        M4F s t   -> E2 s t

decodeMSiki :: Siki -> MSiki
decodeMSiki = anaMSiki phi where
    phi = \ case
        E2 s t -> case s of
            E1     -> case t of
                E1     -> M2F
                E2 a b -> M1F (a,b)
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
(V(●.(●.●)),(),#V(●.(●.●)),(#V(●.(●.●)).()))
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
>>> read @Lst "(V(●.(●.●)),(),#V(●.(●.●)),(#V(●.(●.●)).()))"
(V(●.(●.●)),(),#V(●.(●.●)),(#V(●.(●.●)).()))
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
(V(●.(●.●)).(().(#V(●.(●.●)).((#V(●.(●.●)).()).()))))

>>> decodeLst (read "(V(●.(●.●)).(().(#V(●.(●.●)).((#V(●.(●.●)).()).()))))")
(V(●.(●.●)),(),#V(●.(●.●)),(#V(●.(●.●)).()))
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
#V(●.(●.●))
>>> M1 E1 E1 ∈ sampleLst2
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
(V(●.(●.●)),(),#V(●.(●.●)),(#V(●.(●.●)).()))
>>> sampleLst2 ⊕ sampleLst1
(V(●.(●.●)),(),#V(●.(●.●)),(#V(●.(●.●)).()))
>>> sampleLst1 ⊕ sampleLst2 == sampleLst2 ⊕ sampleLst1
True
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
((),#V(●.(●.●)),(#V(●.(●.●)).()))

>>> sampleLst2 ⊖ sampleMSiki2
(V(●.(●.●)),#V(●.(●.●)),(#V(●.(●.●)).()))

>>> sampleLst2 ⊖ sampleMSiki3
(V(●.(●.●)),(),(#V(●.(●.●)).()))
>>> sampleLst2 ⊖ sampleMSiki4
(V(●.(●.●)),(),#V(●.(●.●)))
>>> (sampleLst2 ⊕ sampleLst2) ⊖ sampleMSiki4
(V(●.(●.●)),(),#V(●.(●.●)),V(●.(●.●)),(),#V(●.(●.●)))

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
vV :: MSiki -> Lst
vV = cataMSiki phi where
    phi = \ case
        M1F v   -> Cns (M1 v) Nl
        M2F     -> Nl
        M3F s   -> s
        M4F s t -> s ⊕ t

vV1 :: MSiki -> Lst
vV1 = cataMSiki phi where
    phi = \ case
        M1F v   -> Cns (M1 v) Nl
        M2F     -> Nl
        M3F _   -> Nl
        M4F s t -> s ⊕ t

vV2 :: MSiki -> Lst
vV2 = paraMSiki phi where
    phi = \ case
        M1F _     -> Nl
        M2F       -> Nl
        M3F (s,_) -> vV s
        M4F (_,s) (_,t) -> s ⊕ t
--
-- ## C式
--
isCSiki :: MSiki -> Bool
isCSiki = cataMSiki phi where
    phi = \ case
        M1F _   -> False
        M2F     -> True
        M3F s   -> s
        M4F s t -> s && t

newtype CSiki = CSiki_ MSiki

encodeCSiki :: CSiki -> MSiki
encodeCSiki (CSiki_ s) = s

decodeCSiki :: MSiki -> CSiki
decodeCSiki = CSiki_ . (bool (error "csiki: Not CSiki") <*> isCSiki)
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
instance Show SSiki where
    showsPrec _ = paraSSiki phi where
        phi = \ case
            S1F     -> showParen True id
            S2F (s,ss) (t,ts) 
                | s == S1 && t == S1 -> showChar '□'
                | otherwise          -> showParen True (ss . showChar '.' . ts)

instance Read SSiki where
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
--
-- ## 代入と抽象化
--
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
subst :: MSiki -> Variable -> MSiki -> MSiki
subst s x t = replace s x (M3 t)

substitute :: MSiki -> [(Variable, MSiki)] -> MSiki
substitute = foldl' phi where
    phi s (x, t) = subst s x t
--
-- ### 出現位置
--
{- |
>>> x = sampleVariable1
>>> y = sampleVariable2
>>> s = M4 (M4 (M1 x) M2) (M4 (M1 x) (M3 (M1 x)))
>>> occ x s
((□.()).(□.()))
-}
sqr :: SSiki
sqr = S2 S1 S1

occ :: Variable -> MSiki ->  SSiki
occ x = cataMSiki phi where
    phi = \ case
        M1F v
            | x == v    -> sqr
            | otherwise -> S1
        M2F             -> S1
        M3F _           -> S1
        M4F s t         -> case (s, t) of
            (S1,S1)     -> S1
            _           -> S2 s t
--
-- ### 抽象化
--
{- |
>>> x = sampleVariable1
>>> y = sampleVariable2
>>> s = M4 (M1 x) (M4 (M1 y) (M3 (M1 x)))
>>> s
(V(●.●).(V((●.●).●).#V(●.●)))
>>> abstract x s
((□.()).(().(V((●.●).●).#V(●.●))))
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
>>> x = sampleVariable1
>>> y = sampleVariable2
>>> s = M4 (M1 x) (M4 (M1 y) (M3 (M1 x)))
>>> abxs = abstract x s
>>> abxs
((□.()).(().(V((●.●).●).#V(●.●))))
>>> isAbsPair abxs
True
>>> abp = decodeAbsPair abxs
>>> abp
A3 □ () () (V((●.●).●).#V(●.●))
>>> instantiation abp (M1 x)
(V(●.●).(V((●.●).●).#V(●.●)))
>>> s
(V(●.●).(V((●.●).●).#V(●.●)))
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
-- サンプルデータ
--
sampleHs :: Hs
sampleHs = [Siro,Siro,Kuro,Siro,Kuro,Kuro,Siro,Siro,Kuro,Kuro,Kuro]

sampleSiki :: Siki
sampleSiki  = E2 (E2 E1 (E2 E1 E1)) (E2 (E2 E1 E1) E1)

sampleVariable1 :: Variable
sampleVariable1 = (E1, E1)

sampleVariable2 :: Variable
sampleVariable2 = (E2 E1 E1, E1)

sampleMSiki1 :: MSiki
sampleMSiki1 = M1 (E1, (E2 E1 E1))

sampleMSiki2 :: MSiki
sampleMSiki2 = M2

sampleMSiki3 :: MSiki
sampleMSiki3 = M3 sampleMSiki1

sampleMSiki4 :: MSiki
sampleMSiki4 = M4 sampleMSiki3 sampleMSiki2

sampleLst1 :: Lst
sampleLst1 = Nl

sampleLst2 :: Lst
sampleLst2 = Cns sampleMSiki1 (Cns sampleMSiki2 (Cns sampleMSiki3 (Cns sampleMSiki4 Nl)))
