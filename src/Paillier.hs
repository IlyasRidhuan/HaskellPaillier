{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Paillier where

import Crypto.Random
import Crypto.Number.Prime
import Crypto.Number.ModArithmetic
import Crypto.Number.Generate

import Control.Monad.Except


data PaillierError = NoModuloInverse -- Couldnt find modulo inverse

instance Show PaillierError where
    show NoModuloInverse = "Could not find a multiplicative modulo inverse"

data PublicKey = PublicKey {
    n :: Integer,
    g :: Integer
} deriving Show

data PrivateKey = PrivateKey {
    λ :: Integer,
    μ :: Integer
} deriving Show

newtype PlainText = PlainText Integer deriving (Show,Num,Enum,Integral,Real,Ord,Eq)
newtype CipherText = CipherText Integer deriving (Show,Num,Enum,Integral,Real,Ord,Eq)

type PaillierMonad = ExceptT PaillierError IO

genKeys :: Int -> PaillierMonad (PublicKey,PrivateKey)
genKeys bits = do
    -- Maybe should validate that gcd(pq,(p-1)(q-1)) == 1
    p <- liftIO $ generatePrime bits
    q <- liftIO $ generatePrime bits
    let n = p*q
    let nSquare = n*n
    let λ = lcm (p-1) (q-1)
    g <- liftIO $ generateMax (nSquare -1) >>= generateValidG nSquare
    -- let g = n + 1
    -- more advanced g <= n^2 such that gcd(g,n^2) == 1
    -- a = L(g^λ `mod` n^2)
    let pubKey = PublicKey { n, g }
    case inverse (lPaillier pubKey (expSafe g λ (n*n))) n of
        Just μ -> return (pubKey, PrivateKey { λ, μ })
        Nothing -> throwError NoModuloInverse

encrypt :: PlainText -> PublicKey -> IO CipherText
encrypt (PlainText m) pk@PublicKey{..} = do
    let nSquared = n*n
    let g_m = expSafe g m nSquared
    r   <- generateMax (n-1) >>= generateValidR pk
    let r_n = expSafe r n nSquared
    return $ CipherText $ expSafe (g_m * r_n) 1 nSquared

-- A valid r is one that is less than and relatively prime to n, i.e gcd (r,n) == 1
generateValidR :: PublicKey -> Integer -> IO Integer
generateValidR pk@PublicKey{..} rCandidate
    | gcd rCandidate n == 1 = return rCandidate
    | otherwise = generateMax (n-1) >>= generateValidR pk

decrypt :: CipherText -> PrivateKey -> PublicKey -> PlainText
decrypt (CipherText c) PrivateKey{..} pub@PublicKey{..} =
    PlainText $ expSafe (lPaillier pub (expSafe c λ (n*n)) * μ) 1 n

lPaillier :: PublicKey -> Integer -> Integer
lPaillier PublicKey{..} x = (x-1) `div` n

generateValidG :: Integer -> Integer -> IO Integer
generateValidG nSquare gCandidate
    | gcd gCandidate nSquare == 1 = return gCandidate
    | otherwise = generateMax (nSquare-1) >>= generateValidG nSquare
