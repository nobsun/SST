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
module Interaction.Responder
    ( responder
    ) where

import Data.Char
import Data.Maybe

{- |
任意の入力列をそれぞれの入力文字列を"なんか関数"に変換
>>> putStr $ unlines $ responder undefined ["Hi.", "お元気ですか？"]
なんか関数
なんか関数
-}

responder :: String -> [String] -> [String]
responder extra = mapMaybe (\ st -> st.output) . eval . initial extra

data MachineState
    = MachineState
    { inChan :: [String]
    , output :: Maybe String
    , innerState :: InnerState
    }

data InnerState

initial :: String -> [String] -> MachineState
initial extra inputs
    = MachineState
    { inChan = inputs
    , output = Nothing
    , innerState = error "initial InnerState is not implemented" extra
    }

eval :: MachineState -> [MachineState]
eval state = state : rests
    where
        rests | isFinal state = []
              | otherwise     = eval (step state)

isFinal :: MachineState -> Bool
isFinal state = case state of
    MachineState { inChan = [] } -> True
    _                            -> False

step :: MachineState -> MachineState
step state = case state of
    MachineState { inChan = i:is }
        -> state { inChan     = is
                 , output     = Just (const "なんか関数" i)
                 , innerState = state.innerState
                 }
    _   -> error "already final"
