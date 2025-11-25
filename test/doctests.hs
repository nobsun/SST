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
module Main where

import Test.DocTest

main :: IO ()
main = doctest 
     ["src/Expression/Hosi.hs"
     ,"src/Expression/Siki.hs"
     ,"src/Expression/Variable.hs"
     ,"src/Expression/MSiki.hs"
     ,"src/Expression/Lst.hs"
     ,"src/Expression/CSiki.hs"
     ,"src/Expression/SSiki.hs"
     ,"src/Expression/Abstraction.hs"
     ,"src/Expression/Substitution.hs"
     ]

