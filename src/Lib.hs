{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Lib where

import Data.Void
import Control.Applicative
import Data.Functor.Rep

type Equal a b = (a -> b, b -> a)

-- http://www.math.com/school/subject2/lessons/S2U2L2DP.html

-- Implies that equality of types is expressed when x^y == y^x  ??

------------------------------------------
-- Identity
------------------------------------------

-- 1 = 1
identity :: Equal () ()
identity = (to, from)
    where
      to () = ()
      from () = ()

------------------------------------------
-- Addition
------------------------------------------

-- 1 + 1 = 2
addition :: Equal (Either () ()) Bool
addition = (to, from)
    where
      to (Left ()) = False
      to (Right ()) = True
      from False = Left ()
      from True = Right ()

-- x + (y + z) == (x + y) + z
additionAssoc :: Equal (Either a (Either b c)) (Either (Either a b) c)
additionAssoc = (to, from)
  where
    to (Left a) = Left (Left a)
    to (Right (Left b)) = Left (Right b)
    to (Right (Right c)) = Right c
    from (Left (Left a)) = Left a
    from (Left (Right b)) = Right (Left b)
    from (Right c) = Right (Right c)

-- x + y == y + x
additionCommut :: Equal (Either a b) (Either b a)
additionCommut = (to, from)
  where
    to (Left a) = Right a
    to (Right b) = Left b
    from (Left b) = Right b
    from (Right a) = Left a

------------------------------------------
-- Multiplication
------------------------------------------

-- a * 2 = a + a
multiplication :: Equal (a, Bool) (Either a a)
multiplication = (to, from)
    where
      to (a, False) = Left a
      to (a, True) = Right a
      from (Left a) = (a, False)
      from (Right a) = (a, True)

multiplicativeDistribution :: Equal (Either a b, c) (Either (a, c) (b, c))
multiplicativeDistribution = (to, from)
  where
    to (Left a, c) = (Left (a, c))
    to (Right b, c) = (Right (b, c))
    from (Left (a, c)) = (Left a, c)
    from (Right (b, c)) = (Right b, c)

-- x * (y * z) == (x * y) * z
multiplicationAssoc :: Equal (a, (b, c)) ((a, b), c)
multiplicationAssoc = (to, from)
  where
    to (a, (b, c)) = ((a, b), c)
    from ((a, b), c) =  (a, (b, c))

-- x * y == y * x
multiplicationCommut :: Equal (Either a b) (Either b a)
multiplicationCommut = (to, from)
  where
    to (Left a) = Right a
    to (Right b) = Left b
    from (Left b) = Right b
    from (Right a) = Left a

------------------------------------------
-- Exponentiation
------------------------------------------

-- 1 == a^0
zeroRule :: Equal () (Void -> a)
zeroRule = (to, from)
    where
      to () = absurd
      from _ = ()

-- a == a^1
aToTheOne :: Equal a (() -> a)
aToTheOne = (to, from)
    where
      to a = const a
      from f = f ()

-- a * a == a^2
aSquared :: Equal (a, a) (Bool -> a)
aSquared = (to, from)
    where
      to (a, a') = \case
        False -> a
        True -> a'
      from f = (f False, f True)


-- 2 * 2 == 2^2
twoSquared :: Equal (Bool, Bool) (Bool -> Bool)
twoSquared = (to, from)
    where
      to (False, False) = const False
      to (False, True) = id
      to (True, False) = not
      to (True, True) = const True
      from f = (f False, f True)

-- b^a * c^a == (b*c)^a
exponentialDistribution :: Equal (a -> b, a -> c) (a -> (b, c))
exponentialDistribution = (to, from)
  where
    to (aToB, aToC) = liftA2 (,) aToB aToC -- 'distribute' or 'sequence' over Pair ((->) a b)
    from aToBC = (fmap fst aToBC, fmap snd aToBC)

-- a^b * a^c == a^(b + c)
productRule :: Equal (b -> a, c -> a) (Either b c -> a)
productRule = (to, from)
  where
    to (bToA, cToA) = \case
        Left b -> bToA b
        Right c -> cToA c
    from eitherToA = (eitherToA . Left, eitherToA . Right)

-- a^(b^c) == a^(b*c)
powerRule :: Equal (b -> c -> a) ((b, c) -> a)
powerRule = (to, from)
  where
    to = uncurry
    from = curry

-- We can witness the relationship between (x * x ~~ x^2)
-- in general as a Representable Functor; e.g. (x, x) ~~ Bool -> x

------------------------------------------
-- Derivation
------------------------------------------

-- type family Derivative term x where
--   Derivative (y -> x) x = (x, y)
--   Derivative x x = ()
--   Derivative y x = Void

type family Derivative term where
  Derivative (f x) = (Rep f, x)


-- d(x^2) == 2x
-- derivation1 :: Representable f => Equal (Derivative (f x)) (Bool, x)
-- derivation1 = (id, id)

-- (x, x) -> (Bool, x)
-- Either (x, x) x  -> Either (Bool, x) ()
-- Either (Bool -> x) x  -> Either (Bool, x) ()
--
-- (x, x, x) -> (Maybe Bool, x, x)
-- (x, x, x, Bool) -> (Maybe Bool, x, Bool)
-- ((x, x, x), Bool) -> map  Product to Sum for x's -> (Either x (Either x x), Bool)
-- (Maybe Bool -> x, Bool) -dx> ((Maybe Bool, x), Bool)

-- x * x * x * 2

-- d(x^2 + x + 1) == 2x + 1
--  Either (x, x) (Either x ())
-- > throw out any constants
-- Either (x, x) x
-- > Use representable to convert to function
--
-- type Poly x = Either (x, x) (Either x ())
--   ~~ ()
--
-- derivation1 :: Equal (Derivative (Bool -> x)) (Bool, x)
-- derivation1 = (id, id)

-- -- d(2x^2) == 4x
-- derivation2 :: Equal (_ (Bool -> (Bool, x)) (Bool, Bool, x)
-- derivation2 = undefined


------------------------------------------
-- Division
------------------------------------------

-- Subtraction cancels terms in a Sum;
-- Division cancels terms in a Product;

-- type family Division numerator denominator =

-- (2 * a) * (1/2) = a
-- division :: Equal ((Bool, a), MultiplicativeInverse 2) a
-- division = (to, from)
--   where
--     to _ = undefined
--     from _ = undefined



------------------------------------------
-- Subtraction
------------------------------------------

-- Subtraction of types must allow us to REMOVE a constructor from a Sum type
-- via evidence of another Sum type;
-- meaning we need to write the isomorphism `Equal (Either a (Neg a)) Void`,
-- which I don't believe is possible to express.
--
-- Perhaps we can still witness it at the type level by writing type families over SOP's?
