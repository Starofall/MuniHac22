{-# language OverloadedLists #-}

module LovecraftLetter.LovecraftDist where

    --- Begin Gaby's Code ---
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Exts (IsList)

import qualified Control.Monad              as Monad
import qualified Data.List.NonEmpty         as NonEmpty

data Possibility a
    = Possibility { outcome :: a, weight :: !Rational }
    deriving (Functor, Show)

-- | A probability distribution, which is a non-empty list of weighted outcomes
newtype Distribution a
    = Distribution { possibilities :: NonEmpty (Possibility a) }
    deriving stock (Functor)
    deriving newtype (IsList)

instance Show a => Show (Distribution a) where
    show distribution = show (NonEmpty.toList (possibilities distribution))

instance Applicative Distribution where
    pure x = Distribution (pure (Possibility x 1))

    (<*>) = Monad.ap

instance Monad Distribution where
    m >>= f = Distribution do
        Possibility x weight0 <- possibilities m

        Possibility y weight1 <- possibilities (f x)

        return $! Possibility y (weight0 * weight1)

-- | Compute the expected value for a probability distribution
expectedValue :: Fractional number => Distribution number -> number
expectedValue Distribution{ possibilities } = sum (fmap weigh possibilities)
    where
    weigh Possibility{ outcome, weight } = fromRational weight * outcome

--- End Gaby's Code ---

data CardValue = One | Two | Three | Four | Five | Six | Seven | Eight deriving(Show, Eq, Ord)

cardValue' :: Distribution CardValue
cardValue' = [Possibility One (5/16), 
            Possibility Two (2/16), 
            Possibility Three (2/16), 
            Possibility Four (2/16), 
            Possibility Five (2/16),
            Possibility Six (1/16), 
            Possibility Seven (1/16), 
            Possibility Eight (1/16)] :: Distribution CardValue

