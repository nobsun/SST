-- # Lambda.Term
-- 項
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
module Lambda.Term
     where
-- 
-- ## import 宣言
--
import Data.Functor.Foldable
import Text.ParserCombinators.ReadP
import Expression
--
-- ## 項とプログラム
--
data Term
    = Var Variable
    | Quote Term
    | Null0
    | IsNull0 Term
    | Pair Term Term
    | IsPair Term
    | Car Term
    | Cdr Term
    | Lambda Variable Term
    | IsFun Term
    | Apply Term Term
    | IsEqual Term Term
    | If Term Term Term
    deriving (Eq,Ord)

data TermF r
    = VarF Variable
    | QuoteF r
    | Null0F
    | IsNull0F r
    | PairF r r
    | IsPairF r
    | CarF r
    | CdrF r
    | LambdaF Variable r
    | IsFunF r
    | ApplyF r r
    | IsEqualF r r
    | IfF r r r
    deriving (Eq,Ord,Functor)

type instance Base Term = TermF

instance Recursive Term where
    project :: Term -> TermF Term
    project = \ case
        Var v       -> VarF v
        Quote t     -> QuoteF t
        Null0       -> Null0F
        IsNull0 t   -> IsNull0F t
        Pair s t    -> PairF s t
        IsPair t    -> IsPairF t
        Car t       -> CarF t
        Cdr t       -> CdrF t
        Lambda v t  -> LambdaF v t
        IsFun t     -> IsFunF t
        Apply s t   -> ApplyF s t
        IsEqual s t -> IsEqualF s t
        If c s t    -> IfF c s t

cataTerm :: (TermF a -> a) -> Term -> a
cataTerm = cata

paraTerm :: (TermF (Term, a) -> a) -> Term -> a
paraTerm = para

instance Corecursive Term where
    embed :: TermF Term -> Term
    embed = \ case
        VarF v       -> Var v
        QuoteF t     -> Quote t
        Null0F       -> Null0
        IsNull0F t   -> IsNull0 t
        PairF s t    -> Pair s t
        IsPairF t    -> IsPair t
        CarF t       -> Car t
        CdrF t       -> Cdr t
        LambdaF v t  -> Lambda v t
        IsFunF t     -> IsFun t
        ApplyF s t   -> Apply s t
        IsEqualF s t -> IsEqual s t
        IfF c s t    -> If c s t

anaTerm :: (a -> TermF a) -> a -> Term
anaTerm = ana

apoTerm :: (a -> TermF (Either Term a)) -> a -> Term
apoTerm = apo
--
-- ### `Show`と`Read`
--
instance Show Term where
    showsPrec :: Int -> Term -> ShowS
    showsPrec _ = cataTerm phi where
        phi = \ case
            VarF v       -> shows v
            QuoteF t     -> showChar '#' . showParen True t
            Null0F       -> showChar '0'
            IsNull0F t   -> ("null?" ++) . showParen True t
            PairF s t    -> showChar '[' . s . showChar '.' . t . showChar ']'
            IsPairF t    -> ("pair?" ++) . showParen True t
            CarF t       -> ("car" ++) . showParen True t
            CdrF t       -> ("cdr" ++) . showParen True t
            LambdaF v t  -> ("λ " ++) . shows (M1 v) . showChar '.' . t
            IsFunF t     -> ("fun?" ++) . showParen True t
            ApplyF s t   -> ("apply" ++) . showParen True (s . showChar ',' . t)
            IsEqualF s t -> ("eq?" ++) . showParen True (s . showChar ',' . t)
            IfF c s t    -> ("if" ++) . showParen True (c . showChar ',' . s . showChar ',' . t)

instance Read Term where
    readsPrec :: Int -> ReadS Term
    readsPrec _ = readP_to_S rTerm

rTerm :: ReadP Term
rTerm = rVar
   +++ rQuote
   +++ rNull0
   +++ rIsNull0
   +++ rPair
   +++ rIsPair
   +++ rCar
   +++ rCdr
   +++ rLambda
   +++ rIsFun
   +++ rApply
   +++ rIsEqual
   +++ rIf

rVar :: ReadP Term
rVar = Var <$> rVariable

rQuote :: ReadP Term
rQuote = Quote <$ char '#' <*> between (char '(') (char ')') rTerm

rNull0 :: ReadP Term
rNull0 = Null0 <$ char '0'

rIsNull0 :: ReadP Term
rIsNull0 = IsNull0 <$ string "null?" <*> between (char '(') (char ')') rTerm

rPair :: ReadP Term
rPair = Pair <$ char '[' <*> rTerm <* char '.' <*> rTerm <* char ']'

rIsPair :: ReadP Term
rIsPair = IsPair <$ string "pair?" <*> between (char '(') (char ')') rTerm

rCar :: ReadP Term
rCar = Car <$> between (char '(') (char ')') rTerm

rCdr :: ReadP Term
rCdr = Cdr <$> between (char '(') (char ')') rTerm

rLambda :: ReadP Term
rLambda = Lambda <$ string "λ " <*> rVariable <* char '.' <*> rTerm

rIsFun :: ReadP Term
rIsFun = IsFun <$> between (char '(') (char ')') rTerm

rApply :: ReadP Term
rApply = Apply <$ string "apply" <* char '(' <*> rTerm <* char ',' <*> rTerm <* char ')'

rIsEqual :: ReadP Term
rIsEqual = IsEqual <$ string "eq?" <* char '(' <*> rTerm <* char ',' <*> rTerm <* char ')'

rIf :: ReadP Term
rIf = If <$ string "if" <* char '(' <*> rTerm <* char ',' <*> rTerm <* char ',' <*> rTerm <* char ')'
