
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module NanoLens.TH where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Eq
import Data.Functor
import Data.Functor.Identity
import Data.Functor.Const
import Data.Function
import Data.List
import GHC.Int
import Text.Show

import NanoLens.Lens

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib



-- | Automatically generate a lens for a numbered field in a record.
--
genNumberedLens :: Name -> Con -> Int -> Q Dec
genNumberedLens lensName con nr = do
    let (conName, fields) = case con of
            RecC nm fs -> (nm, fs)
        fieldNames = map (\(nm, _, _) -> nm) fields
        fieldName  = fieldNames !! nr

    kName <- newName "k"
    sName <- newName "s"
    xName <- newName "x"
    yName <- newName "y"
    let kP = varP kName
        sP = varP sName
        sE = varE sName

        argsP = [ kP, sP ]

        fieldP = [ if nr == n then varP xName else wildP
                 | (n, _) <- zip [0..] fields ]
        recP = conP conName fieldP
        recD = valD recP (normalB sE) []

    let k = varE kName
        x = varE xName
        y = varP yName

        upd1 = (,) fieldName <$> varE yName
        upd  = recUpdE sE [upd1]
        body = [| $k $x <&> \ $y -> $upd |]

    lensDec <- funD lensName
        [ clause argsP (normalB body) [recD] ]
    return lensDec

-- | Automatically derive lenses for all fields in a datatype.
--
genAllLenses :: Name -> Q [Dec]
genAllLenses tyName = do
    TyConI tyDec <- reify tyName
    case tyDec of
        DataD _ _ _ _ [con] _ -> go con
  where
    go con@(RecC nm fields) = do
        let lensNames = genLensNames fields
        lenses <- sequence $
            zipWith (\nm nr -> genNumberedLens nm con nr) lensNames [0..]
        return lenses

    genLensNames fields =
        zipWith (\n _ -> mkName $ "lens" ++ show @Int n) [0..] fields


