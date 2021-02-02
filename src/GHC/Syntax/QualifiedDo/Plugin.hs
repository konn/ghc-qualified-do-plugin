{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module GHC.Syntax.QualifiedDo.Plugin
  ( plugin,
  )
where

import Data.Char
import Data.Generics
import Debug.Trace
import GHC.Hs
import GHC.Hs.Expr (HsExpr)
import GhcPlugins
import TcEvidence (HsWrapper (WpHole))

plugin :: Plugin
plugin =
  defaultPlugin
    { pluginRecompile = purePlugin
    , parsedResultAction = const processQDo
    }

processQDo :: ModSummary -> HsParsedModule -> Hsc HsParsedModule
processQDo _ HsParsedModule {..} =
  pure
    HsParsedModule
      { hpm_module =
          everywhere (mkT rewriteInfixDotDo) hpm_module
      , ..
      }

rewriteInfixDotDo :: LHsExpr GhcPs -> LHsExpr GhcPs
rewriteInfixDotDo
  ( L
      loc
      ( OpApp
          _
          (L _ (HsVar _ (L _ modl)))
          (L _ (HsVar _ (L _ (Unqual nam))))
          (L _ (HsDo _ DoExpr lstmts))
        )
    )
    | occNameString nam == "."
      , let modName = mkModuleName $ showSDocUnsafe $ ppr modl
      , isUpper $ head $ occNameString $ rdrNameOcc modl =
      desugarQDo loc modName lstmts
rewriteInfixDotDo e = e

desugarQDo :: SrcSpan -> ModuleName -> Located [ExprLStmt GhcPs] -> LHsExpr GhcPs
desugarQDo _ modName (L _ lstmts)
  | not $ null lstmts
    , let len = length lstmts
    , (ini, [L _ (getStandaloneE -> Just lastE)]) <- splitAt (len - 1) lstmts =
    foldr step lastE ini
  where
    unhelp = UnhelpfulSpan "<desugared>"
    bind, andThen, failing :: HsExpr GhcPs
    bind = HsVar noExtField $ L unhelp $ Qual modName $ mkVarOcc ">>="
    andThen = HsVar noExtField $ L unhelp $ Qual modName $ mkVarOcc ">>"
    failing = HsVar noExtField $ L unhelp $ Qual modName $ mkVarOcc "fail"

    step :: ExprLStmt GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
    step (L lHere st) k = L entireSpan $ case st of
      LastStmt {} -> error "Could not happen!"
      (BindStmt _ pat@(L l _) val _ failSynExp)
        | isVarPat pat ->
          OpApp noExtField val (L entireSpan bind) $
            L l $
              HsLam noExtField $
                MG
                  noExtField
                  ( L
                      (getLoc k)
                      [ L (getLoc k) $
                          Match noExtField LambdaExpr [pat] $
                            GRHSs
                              noExtField
                              [L (getLoc k) $ GRHS noExtField [] k]
                              (L l $ EmptyLocalBinds noExtField)
                      ]
                  )
                  Generated
        | otherwise ->
          OpApp noExtField val (L entireSpan bind) $
            L entireSpan $
              HsLamCase noExtField $
                MG
                  noExtField
                  ( L entireSpan $
                      L
                        entireSpan
                        ( Match noExtField LambdaExpr [pat] $
                            GRHSs
                              noExtField
                              [L (getLoc k) $ GRHS noExtField [] k]
                              (L unhelp $ EmptyLocalBinds noExtField)
                        ) :
                        [ L l $
                          Match noExtField LambdaExpr [L l $ WildPat noExtField] $
                            GRHSs
                              noExtField
                              [ L l $
                                  GRHS
                                    noExtField
                                    []
                                    ( L l $
                                        HsApp noExtField (L l failing) $
                                          L l $
                                            HsLit noExtField $
                                              HsString NoSourceText $
                                                fsLit $
                                                  "pattern match failure at: "
                                                    Prelude.<> show l
                                    )
                              ]
                              (L unhelp $ EmptyLocalBinds noExtField)
                        | not $ noFail failSynExp
                        ]
                  )
                  Generated
      ApplicativeStmt {} -> error "ApplicativeDo"
      BodyStmt _ e _ _ ->
        OpApp noExtField e (L entireSpan andThen) k
      (LetStmt _ binds) -> HsLet noExtField binds k
      ParStmt {} -> error "Cannot appear: ParStmt"
      TransStmt {} -> error "I don't know what to do: TransStmt"
      RecStmt {} -> error "RecStmt not supported yet"
      XStmtLR {} -> error "Cannot reach here"
      where
        entireSpan =
          mkSrcSpan (srcSpanStart lHere) (srcSpanEnd $ getLoc k)
desugarQDo loc _ stmts = L loc $ HsDo noExtField DoExpr stmts

getStandaloneE :: StmtLR GhcPs GhcPs (LHsExpr GhcPs) -> Maybe (LHsExpr GhcPs)
getStandaloneE = \case
  (LastStmt _ b _ _) -> Just b
  (BodyStmt _ b _ _) -> Just b
  _ -> Nothing

noFail :: SyntaxExpr GhcPs -> Bool
noFail (SyntaxExpr (HsLit _ (HsString _ "noSyntaxExpr")) [] WpHole) = True
noFail _ = False

isVarPat :: LPat GhcPs -> Bool
isVarPat (unLoc -> VarPat {}) = True
isVarPat _ = False
