{-# LANGUAGE ConstraintKinds #-}

module Shamir
( ShareIdPair(..)
, ShamirParams(..)
, validParams
, share
, recover
, beta
)
where

import Prelude hiding ((!!), length, take)
import System.Random
-- import Test.QuickCheck

-- import Math.Divisibility
-- import Math.BigInt
-- import Math.Gen
-- import Math.Prime
-- import Math.NumClass
-- import Tools
import Data.Bits

-- The generic type used in most places need these three constraints.
type NumClass a = (Bits a, Integral a, Random a)

-- Couples a share si to the ID pi.
data ShareIdPair a = ShareIdPair a a
                   deriving (Eq)

instance Show a => Show (ShareIdPair a) where
  show (ShareIdPair s p) = "<share=" ++ 
                            show s ++ 
                            ",id=" ++ 
                            show p ++ 
                            ">"

-- Parameters n (number of parties), t (threshold) and q (modulo).
data ShamirParams a = ShamirParams a a a
                    deriving (Eq)

instance Show a => Show (ShamirParams a) where
  show (ShamirParams n t q) = "ShamirParams n=" ++ 
                              show n ++ 
                              " t=" ++ 
                              show t ++ 
                              " q=" ++ 
                              show q

validParams :: NumClass a => ShamirParams a -> Bool
validParams (ShamirParams n t q) = t >= 1
                                && n > t
                                && q >= 2
                                && primeDet q
                                && q > n
-- In the beta function, inverses of numbers up to n will be
-- calculated mod q. So if q is a prime > n all such inverses will exist.


share :: (NumClass a, RandomGen g) => 
  a -> ShamirParams a -> g -> ([ShareIdPair a], g)
share s p@(ShamirParams _ _ q) g
  | s /= s `mod` q      = error "s not in Zq"
  | not (validParams p) = error "Invalid parameters"
  | otherwise           = shareInner s p g

shareInner :: (NumClass a, RandomGen g) => 
  a -> ShamirParams a -> g -> ([ShareIdPair a], g)
shareInner s (ShamirParams n t q) g = (zipWith ShareIdPair sis pis, g'')
  where
    sis      = map polyFun pis
    polyFun  = poly (s:coeffs) q
    coeffs   = take t $ randomRs (0,q-1) g'
    (g',g'') = split g
    pis      = [1..n]

-- poly [c0..cm] q x computes the value of the polynomial
-- function with coefficients c0 to cm, at x, modulo q.
poly :: Integral a => [a] -> a -> a -> a
poly []     _ _ = 0
poly (c:cs) q x = (c + x*poly cs q x) `mod` q

-- prop_poly :: [BigInt7] -> BigInt7 -> BigInt7 -> Property
-- prop_poly cs q x = q >= 2 ==> sum1 == sum2
--   where
--     sum1  = poly cs q x
--     sum2 = (sum $ zipWith (\c e -> c*x^e) cs [0..]) `mod` q


recover :: NumClass a => [ShareIdPair a] -> ShamirParams a -> a
recover pairs (ShamirParams n t q) = sum prods `mod` q
  where
    prods = zipWith (*) bi_s si_s
    bi_s  = map (beta pi_s q) pi_s
    pi_s  = map (\(ShareIdPair _ pi) -> pi) pairs
    si_s  = map (\(ShareIdPair si _) -> si) pairs

-- beta js q i calculates beta i, when parties js are 
-- involved, modulo q.
beta :: Integral a => [a] -> a -> a -> a
beta js q i = product [j * (j-i) `invMod` q | j <- js, j /= i] `mod` q

-- -- prop_beta g t te q, generates a degree t polynomial
-- -- and tests it with t+te beta values, modulo q.
-- prop_beta :: StdGen -> BigInt5 -> BigInt5 -> BigInt5 -> Property
-- prop_beta g t te q = t' >= 1 ==> p0 == p0'
--   where
--     (t', te', q')       = propBetaParams t te q
--     (xVals, coeffs, p0) = propBetaPolynom (t'+1+te') (t'+1) q' g
    
--     yVals = map (poly coeffs q') xVals
--     bVals = map (beta xVals  q') xVals
--     -- polynomial at 0 using beta values
--     p0'   = (sum $ zipWith (multMod q') bVals yVals) `mod` q'

-- instance Arbitrary StdGen where
--   arbitrary = do
--     seed <- arbitrary
--     return $ mkStdGen seed

-- propBetaParams :: NumClass a => a -> a -> a -> (a, a, a)
-- propBetaParams t te q = (t', te', q')
--   where
--     t'  = t  `mod` 40 -- Performance reasons
--     te' = te `mod` 40 -- Performance reasons
--     q'  = nextPrime (max q (t'+te'+2))
--     -- The modulo must be a prime. And all x values must be coprime
--     -- with the modulo, and we will grab x values from 1 to t'+1+te'
--     -- which means we must have at least t'+te'+2 x values because
--     -- they must be unique.

-- propBetaPolynom :: (NumClass a, RandomGen g) => 
--   a -> a -> a -> g -> ([a], [a], a)
-- propBetaPolynom numX numC q g = (xVals, coeffs, p0)
--   where
--     (xVals, g') = randomRUs numX (1, q-1) g
--     coeffs      = take numC $ randomRs (0, q-1) g'
--     p0          = head coeffs


-- divides d n tests if d divides n.
divides :: Integral a => a -> a -> Bool
d `divides` n = n `mod` d == 0

primeDet :: Integral a => a -> Bool
primeDet n
  | n < 2  = False
  | n >= 2 = null divisors
  where
    divisors = filter (`divides` n) [2..n-1]


take :: Integral a => a -> [b] -> [b]
take _ []     = []
take 0 _      = []
take n (a:as) = a:(take (n-1) as)


-- eea a b returns (d,s,t) s.t. d=as+bt. d>0 unless a=b=0.
eea :: Integral a => a -> a -> (a, a, a)
eea a b
  | b == 0         = (abs a,signum a,0)
  | a < 0 || b < 0 = let (d,s,t) = eea (abs a) (abs b)
                     in  (d, signum a * s, signum b * t)
  
  | otherwise      = (d,s,t)
  where
    r         = a `mod` b
    q         = a `div` b
    (d,s',t') = eea b r
    (s,t)     = (t',s'-q*t')

multMod :: Integral a => a -> a -> a -> a
multMod q a b = a*b `mod` q

-- a `invMod` m returns the inverse of a modulo m. 
-- Requirements: a>0, m>=2, a and m coprime.
invMod :: Integral a => a -> a -> a
a `invMod` m
  | d /= 1    = error "Not coprime"
  | otherwise = s `mod` m
  where
    (d,s,_) = eea a m


-- prop_shareRecover :: StdGen -> BigInt5 -> ShamirParams BigInt5 -> Bool
-- prop_shareRecover g s p@(ShamirParams n t q) = s_rec == s'
--   where
--     (s', allPairs, g') = propShareRecoverSecretAndPairs s p g
--     pi_s               = propShareRecoverParties p g
--     pairs = [allPairs !! (pi-1) | pi <- pi_s]
--     s_rec = recover pairs p

-- instance (NumClass a, Arbitrary a) => Arbitrary (ShamirParams a) where
--     arbitrary = do
--       (n,t,q) <- arbitrary
--       let n' =  n `mod` 40      + 2      -- Performance
--           t' = (t `mod` (n'-1)) + 1      -- Performance
--           q' = nextPrime  $ max q (n'+1) -- Correctness
--       return $ ShamirParams n' t' q'

-- propShareRecoverSecretAndPairs :: (NumClass a, RandomGen g) =>
--   a -> ShamirParams a -> g -> (a, [ShareIdPair a], g)
-- propShareRecoverSecretAndPairs s p@(ShamirParams _ _ q) g = 
--   (s', pairs, g')
--   where
--     s' = s `mod` q
--     (pairs, g') = share s' p g

-- propShareRecoverParties :: (NumClass a, RandomGen g) => 
--   ShamirParams a -> g -> [a]
-- propShareRecoverParties (ShamirParams n t _) g = pi_s
--   where
--     (numParties, g') = randomR (t+1, n) g
--     (pi_s, _)        = randomRUs numParties (1,n) g'