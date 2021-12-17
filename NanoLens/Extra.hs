
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module NanoLens.Extra where

import Control.Applicative
import Control.Monad
import Control.Monad.State
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



-- | Alias for 'over'.
--
(%~) :: Concave s t a b -> (a -> b) -> s -> t
(%~) = over

-- | Monadic alias for 'view'.
--
viewM :: MonadState s m => Convex a s a -> m a
viewM l = view l <$> get

-- | Monadic alias for 'over'.
--
(%=) :: MonadState s m => Concave s s a b -> (a -> b) -> m ()
l %= f = modify (l %~ f)

-- | Lenses from a type onto itself.
--
type Lens' s a = Lens s s a a


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


