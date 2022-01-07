
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module NanoLens.MetaTH where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Eq
import Data.Functor
import Data.Functor.Identity
import Data.Functor.Const
import Data.Function
import Data.List
import Data.Maybe
import GHC.Int
import Text.Show

import NanoLens.Lens

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib



-- | All the metadata needed to derive a lens:
-- - the lens's name;
-- - the lens's targets;
-- - the original field type;
-- - the outer type.
--
data LensMeta = LensMeta Name [LensTarget] Type Type

-- | For each constructor in the type we lens over, we
-- target a list of fields of that constructor.
--
data LensTarget = LensTarget Name Int [Int]

-- | Generate a lens signature of the form.
--  >>> lens :: Lens' s a
--
genLensSignature :: LensMeta -> Q Dec
genLensSignature (LensMeta nm _ originTy outerTy) = do
    sigD nm (return $ conAppsT ''Lens' [outerTy, originTy])

genLensFunction :: LensMeta -> Q Dec
genLensFunction (LensMeta nm targets _ _) = do
    funD nm $ map genLensClause targets

-- | Generate a clause for a lens constructor:
--  >>> lens k (Con1 x v) = k x <&> \y -> Con1 y v
--
genLensClause :: LensTarget -> Q Clause
genLensClause (LensTarget conName fieldNum [nr]) = do
    kName <- newName "k"
    let kPat = varP kName
        kExp = varE kName

    vNames <- newNames "v" fieldNum
    let targetPat = conP conName $ map varP vNames
        xExp = varE $ vNames !! nr

    yName <- newName "y" 
    let yPat = varP yName
        yExps = map varE $ zipWith go [0..] vNames where
            go n v = if n == nr then yName else v

        conExp = foldl appE (conE conName) yExps
        targetLam = lamE [ yPat ] conExp

        lensBody = [| $kExp $xExp <&> $targetLam |]

    clause [ kPat, targetPat ] (normalB lensBody) []

genLensClause _ =
    fail "cannot satisfy many-indexed targets"



data LensScheme = LensScheme
    { amendScheme   :: AmendScheme
    , nameScheme    :: LensNameScheme }

type AmendScheme = LensMeta -> Dec -> Dec -> Q [Dec]

type LensNameScheme = Name -> Name

genLensConnective :: LensScheme -> LensMeta -> Q [Dec]
genLensConnective scheme meta = do
    lensSig <- genLensSignature meta
    lensFun <- genLensFunction meta

    amendScheme scheme meta lensSig lensFun

simpleScheme = LensScheme
    { amendScheme   = amendNothing
    , nameScheme    = underscoreNameScheme }
  where
    amendNothing meta sig fun = return [ sig, fun ]

overloadScheme = LensScheme
    { amendScheme   = amendInstance
    , nameScheme    = underscoreNameScheme }
  where
    amendInstance (LensMeta nm _ originTy outerTy) sig fun = do
        classExists <- isJust <$> lookupTypeName (show className)
        classDec <- return $ if classExists
            then []
            else [genClass className nm]
        
        let instanceTy = conAppsT className [outerTy, originTy]
        instanceDec <-
            instanceD (cxt []) (return instanceTy) [ return fun ]

        sequence $ classDec ++ [ return instanceDec ]
      where
        className = case nameBase nm of
            x:xs    -> mkName $ "Has" ++ (toUpper x:xs)
            []      -> mkName ""

    genClass className nm = do
        sName <- newName "s"
        aName <- newName "a"
        let tyVars   = [ PlainTV sName, PlainTV aName ]
            funDeps  = [ FunDep [sName] [aName] ]
            methodTy = conAppsT ''Lens' [ VarT sName, VarT aName ]
        classD (cxt []) className tyVars funDeps
            [ sigD nm (return methodTy) ]

underscoreNameScheme nm = case nameBase nm of
    '_':x:xs    -> mkName (toLower x:xs)
    _           -> mkName ""


-- | Get a list of lens metadata from a type declaration.
--
preprocessTyForLenses :: Dec -> Q [LensMeta]
preprocessTyForLenses (DataD _ _ tyVars _ cons _) = do
    fields <- getFields
    fieldMap = foldl keyField empty fields
  where
    keyField field@(nm, _, _) = insert nm field


-- | Apply a list of TH type arguments to a type constructor.
--
conAppsT :: Name -> [Type] -> Type
conAppsT nm = foldl AppT (ConT nm)

-- | Create a list of @n@ names.
--
newNames :: String -> Int -> Q [Name]
newNames base n =
    sequence [ newName (base ++ show i) | i <- [1..n] ]


