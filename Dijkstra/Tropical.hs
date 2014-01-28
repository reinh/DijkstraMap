{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Dijkstra.Tropical
    ( Tropical(Tropical, getTropical)
    , infinity
    , Semiring
    , StarSemiring
    , Weight
    )
    where

import Control.Applicative
import Control.DeepSeq     (NFData(..))
import Data.Foldable       (Foldable)
import Data.Monoid
import Data.Traversable    (Traversable)

data Tropical a = Tropical { getTropical :: Maybe a }
    deriving (Eq, Functor, Foldable, Traversable)

type Weight = Tropical Int

infinity :: Tropical a
infinity = Tropical Nothing
{-# INLINE infinity #-}

instance NFData a => NFData (Tropical a) where
    rnf = rnf . getTropical

instance Ord a => Ord (Tropical a) where
    Tropical Nothing  `compare` Tropical Nothing  = EQ
    Tropical Nothing  `compare` _                 = GT
    _                 `compare` Tropical Nothing  = LT
    Tropical (Just a) `compare` Tropical (Just b) = a `compare` b
    {-# INLINE compare #-}

instance Ord a => Monoid (Tropical a) where
    mempty = infinity
    {-# INLINE mempty #-}

    mappend = min
    {-# INLINE mappend #-}

instance Applicative Tropical where
    pure = Tropical . Just
    {-# INLINE pure #-}

    Tropical (Just f) <*> Tropical (Just x) = pure (f x)
    _                 <*> _                 = Tropical Nothing

instance Monad Tropical where
    return = pure
    Tropical Nothing  >>= _ = Tropical Nothing
    Tropical (Just a) >>= k = k a

instance Num a => Num (Tropical a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = liftA abs
    signum = liftA signum
    fromInteger = pure . fromInteger

-- Laws:
-- a <+> b = b <+> a
-- (a <+> b) <+> c = a <+> (b <+> c)
-- a <+> zero  = zero <+> a  = a
-- (a <.> b) <.> c = a <.> (b <.> c)
-- a <.> one  = one <.> a  = a
-- a <.> zero = zero <.> a = zero
-- a <.> (b <+> c) = a <.> b <+> a <.> c
-- (a <+> b) <.> c = a <.> c <+> b <.> c
class Semiring a where
    zero  :: a
    plus :: a -> a -> a
    one   :: a
    times :: a -> a -> a

instance (Ord a, Num a) => Semiring (Tropical a) where
    zero = mempty
    plus = mappend
    one  = pure 0
    times a b = (+) <$> a <*> b

-- Laws:
-- star a = one <+> a <.> star a = one <+> star a <.> a
class Semiring a => StarSemiring a where
    star :: a -> a

instance (Ord a, Num a) => StarSemiring (Tropical a) where
    star _ = one

-- Optional Show instance
instance Show a => Show (Tropical a) where
    show (Tropical Nothing)  = "∞"
    show (Tropical (Just a)) = show a
