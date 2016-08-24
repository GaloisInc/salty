{-# LANGUAGE RecordWildCards #-}

module Opt.Simpl where

import TypeCheck.AST

import Language.Slugs.Lens


class Simp a where
  simp :: a -> a

instance Simp a => Simp [a] where
  simp = map simp

instance Simp a => Simp (Maybe a) where
  simp = fmap simp

instance Simp a => Simp (Group a) where
  simp = fmap simp

instance Simp Controller where
  simp Controller { .. } =
    Controller { cInputs      = simp cInputs
               , cOutputs     = simp cOutputs
               , cEnvTrans    = simp cEnvTrans
               , cEnvLiveness = simp cEnvLiveness
               , cSysTrans    = simp cSysTrans
               , cSysLiveness = simp cSysLiveness
               , cFuns        = simp cFuns
               , .. }

instance Simp StateVar where
  simp StateVar { .. } = StateVar { svInit = simp svInit, .. }

instance Simp Fun where
  simp Fun { .. } = Fun { fBody = simp fBody, .. }

instance Simp Expr where
  simp = rewriteOf traverseExpr simpExpr1

-- | Single-step simplification for expressions.
simpExpr1 :: Expr -> Maybe Expr

simpExpr1 (ENot ETrue)  = Just EFalse
simpExpr1 (ENot EFalse) = Just ETrue

simpExpr1 (ENot (ENot e)) = Just e

simpExpr1 (EOr  a b) | a == b = Just a
simpExpr1 (EAnd a b) | a == b = Just a

simpExpr1 (EAnd ETrue e) = Just e
simpExpr1 (EAnd e ETrue) = Just e

simpExpr1 (EAnd EFalse _) = Just EFalse
simpExpr1 (EAnd _ EFalse) = Just EFalse

simpExpr1 (EAnd (ENot a) b) | a == b = Just EFalse
simpExpr1 (EAnd a (ENot b)) | a == b = Just EFalse

simpExpr1 (EOr  EFalse e) = Just e
simpExpr1 (EOr  e EFalse) = Just e

simpExpr1 (EOr ETrue _) = Just ETrue
simpExpr1 (EOr _ ETrue) = Just ETrue

simpExpr1 (EOr (ENot a) b) | a == b = Just ETrue
simpExpr1 (EOr a (ENot b)) | a == b = Just ETrue

simpExpr1 (EEq _ (ENum a) (ENum b)) | a == b    = Just ETrue
                                    | otherwise = Just EFalse

simpExpr1 (EEq _ (EVar _ a) (EVar _ b)) | a == b = Just ETrue

simpExpr1 (EEq _ ETrue  ETrue)  = Just ETrue
simpExpr1 (EEq _ EFalse EFalse) = Just ETrue
simpExpr1 (EEq _ ETrue  EFalse) = Just EFalse
simpExpr1 (EEq _ EFalse ETrue)  = Just EFalse

simpExpr1 (EEq _ a b) | a == b = Just ETrue

simpExpr1 _            = Nothing
