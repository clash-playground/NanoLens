
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module NanoLens.Lens where

import Control.Applicative
import Data.Coerce
import Data.Functor
import Data.Functor.Identity
import Data.Functor.Const
import Data.Function



-- | Compose with a coercion.
--
(#.) :: Coercible c b => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ =
    coerce (\x -> x :: g) :: forall f g. Coercible g f => f -> g
    -- ^ Given that @Coercible c b@ implies @Coercible (a -> c) (a -> b)@,
    -- @(#.) q p@ becomes just a coercion on @id p@.

(.#) :: Coercible c b => (a -> b) -> (b -> c) -> (a -> c)
(.#) p _ = coerce p

-- | Minimal implementation of lenses for convenient field access.
--
-- Typical lenses should look something like:
-- >>> lens :: Functor f => (a -> f b) -> s -> f t
-- >>> lens k (S x) = k x <&> \y -> T y
--
-- Given this definition, observe that
-- >>> lens Const (S x)
-- >>>   == Const x <&> \y -> T y
-- >>>   == Const x
--          (from @Const v <&> f == Const v@)
--
-- Also observe that
-- >>> lens (\_ -> Identity v) (S x)
-- >>>   == (\_ -> Identity v) x <&> \y -> T y
-- >>>   == Identity v <&> \y -> T y
-- >>>   == Identity (T v)
--          (from @Identity v <&> f == Identity (f v)@)
--
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | Convex lenses can /focus in/ on a field of a record.
--
type Convex r s a = (a -> Const r a) -> s -> Const r s

view :: Convex a s a -> a
view l = getConst #. l Const

-- | Concave lenses let the values of a field diverge with changes in
-- incidence.
--
type Concave s t a b = (a -> Identity b) -> s -> Identity t

over :: Concave s t a b -> (a -> b) -> s -> t
over = coerce

set :: Concave s t a b -> b -> s -> t
set l b = runIdentity #. l (\_ -> Identity b)
