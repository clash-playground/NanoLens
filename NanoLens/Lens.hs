
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module NanoLens.Lens where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Coerce
import Data.Eq
import Data.Functor
import Data.Functor.Identity
import Data.Functor.Const
import Data.Function



-- | Nano-implementation of lenses.
--
-- Typical lenses should look something like:
--  >>> lens :: Functor f => (a -> f b) -> s -> f t
--  >>> lens k (S x) = k x <&> \y -> T y
--
-- Given this definition, observe that
--  >>> lens Const (S x)
--  >>>   == Const x <&> \y -> T y
--  >>>   == Const x
-- from @Const v <&> f == Const v@.
--
-- Also observe that
--  >>> lens (\_ -> Identity v) (S x)
--  >>>   == (\_ -> Identity v) x <&> \y -> T y
--  >>>   == Identity v <&> \y -> T y
--  >>>   == Identity (T v)
-- from @Identity v <&> f == Identity (f v)@.
--
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | Implement the standard lens explained in 'Lens' from convex
-- and concave parts.
--
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens convex concave k s = k (convex s) <&> concave s

-- | Convex lenses can /focus in/ on a field of a record.
--
type Convex r s a = (a -> Const r a) -> s -> Const r s

view :: Convex a s a -> s -> a
view l = getConst #. l Const

-- | Concave lenses let the values of a field diverge with changes in
-- incidence.
--
type Concave s t a b = (a -> Identity b) -> s -> Identity t

-- | Apply a function under a lens.
-- 
-- Note that @a -> b ~ a -> Identity b@, so
--  >>> lens (coerce f) (S x)
--  >>>   == (Identity . f) x <&> \y -> T y
--  >>>   == Identity (f x) <&> \y -> T y
--  >>>   == Identity (T $ f x)
--  
over :: Concave s t a b -> (a -> b) -> s -> t
over = coerce

-- | Alias for 'over'.
--
(%~) :: Concave s t a b -> (a -> b) -> s -> t
(%~) = over

-- | Set a value under a lens.
--
-- See 'Lens' for details.
--
set :: Concave s t a b -> b -> s -> t
set l b = runIdentity #. l (\_ -> Identity b)

-- | Alias for 'set'.
--
(.~) :: Concave s t a b -> b -> s -> t
(.~) = set


-- | Endomorphic lenses.
--
-- A lens over a field that doesn't change the record's type is an
-- endomorphism on the record type.
--
type Lens' s a = Lens s s a a

-- | Monadic alias for 'view'.
--
use :: MonadState s m => Convex a s a -> m a
use l = view l <$> get

-- | Monadic alias for 'over'.
--
(%=) :: MonadState s m => Concave s s a b -> (a -> b) -> m ()
l %= f = modify (l %~ f)


-- | Compose with a coercion.
--
(#.) :: Coercible c b => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce (\x -> x :: g)
    :: forall f g. Coercible g f => f -> g
    -- ^ Given
    --  >>> Coercible c b
    --  >>>   => Coercible (a -> c) (a -> b)
    -- @(#.) q p@ becomes just a coercion on @id p@.

(.#) :: Coercible c b => (a -> b) -> (b -> c) -> (a -> c)
(.#) p _ = coerce p

