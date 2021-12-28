
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module NanoLens.TH where

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



-- | Automate all the legwork of generating nanolenses. Any other
-- top-level nanolens generators should call to this with their rules.
--
nanolensify :: Rules -> Name -> Q [Dec]
nanolensify rules tyName = do
    (con, tyVars) <- reifyConAndStuff

    let tyArgs = map tyVarToVar tyVars where
            tyVarToVar (PlainTV nm)     = VarT nm
            tyVarToVar (KindedTV nm _)  = VarT nm
        ty = tyName `conAppsT` tyArgs

        fields = case con of
            RecC nm fs -> fs

    concat <$> (sequence $ genZipper con ty fields)
  where
    genLens con ty   = genLensSupplemental rules con ty
    genZipper con ty = zipWith (genLens con ty) [0..]

    reifyConAndStuff = do
        TyConI tyDec <- reify tyName
        return $ case tyDec of
            DataD _ _ tyVars _ [con] _ -> (con, tyVars)

genSimpleLenses :: Name -> Q [Dec]
genSimpleLenses = nanolensify simpleRules

genLenses :: Name -> Q [Dec]
genLenses = nanolensify classyRules

-- | Automatically generate a lens for a numbered field in a record.
--
genNumberedLens :: Con -> Int -> Name -> Q Dec
genNumberedLens con nr lensName = do
    let (conName, fields) = case con of
            RecC nm fs -> (nm, fs)
        fieldNames = map (\(nm, _, _) -> nm) fields
        fieldName  = fieldNames !! nr

    kName <- newName "k"
    sName <- newName "s"
    let lensParams = [ varP kName, varP sName ]
    -- ^>>> lens k s =

    xName   <- newName "x"
    yName   <- newName "y"
    vNames  <- newNames "v" (length fields - 1)
    let mkFieldWith f n x vs
          | n == nr     = f x : mkFieldWith f (n + 1) x vs
          | otherwise   = case vs of
            v:vs' -> f v : mkFieldWith f (n + 1) x vs'
            []    -> []

        fieldP  = mkFieldWith varP 0 xName vNames
        recP    = conP conName fieldP
        openD   = valD recP (normalB $ varE sName) []

    closeName <- newName "close"
    let fieldE  = mkFieldWith varE 0 yName vNames
        closeE  = foldl appE (conE conName) fieldE
        closeD  = funD closeName
            [ clause [ varP yName ] (normalB closeE) [] ]
    -- ^>>> where
    --  >>>   Rec v1 v2 ... x ... vn
    --  >>>   close y = Rec v1 v2 ... y ... vn

    let k       = varE kName
        x       = varE xName
        close   = varE closeName
        body = [| $k $x <&> $close |]

    lensDec <- funD lensName
        [ clause lensParams (normalB body) [openD, closeD] ]
    return lensDec

-- | Generate a lens with supplemental equipment.
--
-- Some lenses should be defined as class methods to prevent namespace
-- pollution. That will be taken care of here.
--
genLensSupplemental
    :: Rules
    -> Con
    -> Type
    -> Int
    -> (Name, Bang, Type)
    -> Q [Dec]
genLensSupplemental rules con s nr (fieldName, _, fieldTy) = do
    fieldClassD <- maybeGenFieldClass
    sequence $ lensInstanceD : fieldClassD
  where
    maybeGenFieldClass
      | generateClasses rules = do
        classExists <- isJust <$> lookupTypeName (show className)
        return $ if classExists
            then []
            else [genFieldClass className lensName]
      | otherwise = return []

    lensFunD = genNumberedLens con nr lensName
    lensInstanceD
      | generateClasses rules =
        genFieldInstance s fieldTy className lensFunD
      | otherwise = lensFunD

    lensName    = fieldNamer rules fieldName
    className   = classNamer rules lensName

-- | Generate a multi-param type class for a field with a particular name.
--
-- For example:
--  >>> data Bar = Bar { _foo :: Field1 }
--  >>>
--  >>> class HasFoo s a where
--  >>>     foo :: Lens' s a
--  >>>
--  >>> instance HasFoo Bar Field1 where
--  >>>     foo = lens (_foo) (\x b -> x {_foo = b})
--
genFieldClass :: Name -> Name -> Q Dec
genFieldClass className lensName = do
    s <- newName "s"
    a <- newName "a"

    let sTyVar  = PlainTV s
        sTy     = VarT s
        aTyVar  = PlainTV a
        aTy     = VarT a

        methodTy = ''Lens' `conAppsT` [sTy, aTy]

    classD (cxt []) className [sTyVar, aTyVar] [FunDep [s] [a]]
        [ sigD lensName (return methodTy) ]

-- | Generate the instance corresponding to a field type class.
--
genFieldInstance :: Type -> Type -> Name -> Q Dec -> Q Dec
genFieldInstance ty fieldTy className lensFunD = do
    instanceD (cxt []) (return $ className `conAppsT` [ty, fieldTy])
        [ lensFunD ]


data Rules = Rules
    { generateClasses   :: Bool
    , classNamer        :: ClassNamer
    , fieldNamer        :: FieldNamer }

type ClassNamer = Name -> Name
type FieldNamer = Name -> Name

simpleRules :: Rules
simpleRules = Rules
    { generateClasses   = False
    , classNamer        = const (mkName "")
    , fieldNamer        = underscorize }

underscorize nm = case nameBase nm of
    '_':x:xs    -> mkName (toLower x:xs)
    _           -> mkName ""

classyRules :: Rules
classyRules = Rules
    { generateClasses   = True
    , classNamer        = classyNamer
    , fieldNamer        = underscorize }
  where
    classyNamer nm = case nameBase nm of
        x:xs -> mkName $ "Has" ++ (toUpper x:xs)
        []   -> mkName ""


-- | Apply a list of TH type arguments to a type constructor.
--
conAppsT :: Name -> [Type] -> Type
conAppsT nm = foldl AppT (ConT nm)

-- | Create a list of @n@ names.
--
newNames :: String -> Int -> Q [Name]
newNames base n =
    sequence [ newName (base ++ show i) | i <- [1..n] ]

