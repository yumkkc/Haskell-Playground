{-# LANGUAGE FlexibleInstances #-}

module VarLang where

import Calc
import qualified Data.Map as M

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
        deriving (Show)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var


instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup


opHelper :: (M.Map String Integer -> Maybe Integer) ->
             (M.Map String Integer -> Maybe Integer) ->
             M.Map String Integer ->
             (Integer -> Integer -> Integer) ->
             Maybe Integer
opHelper l r mp op = case l mp of
                      Nothing -> Nothing
                      Just i1 -> case r mp of
                                   Nothing -> Nothing
                                   Just i2 -> Just $ op i1 i2

instance Num (M.Map String Integer -> Maybe Integer) where
  (+) l r mp = opHelper l r mp (+)
  (*) l r mp = opHelper l r mp (*)
  fromInteger i x = Just i

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = fromInteger
  add  = (+)
  mul = (*)


-- :set -package containers

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

-- testing
test1 = withVars [("x", 6)] $ add (lit 3) (var "x")
test2 = withVars [("x", 6), ("y", 8)] $ mul (add (lit 3) (var "x")) (var "y")
