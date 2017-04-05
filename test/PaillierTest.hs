module PaillierTest where

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.Either
import Control.Monad.Except
import Paillier

testPaillier :: IO ()
testPaillier = do
    bits <- generate $ abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 5)
    Right(pub,prv) <- runExceptT $ genKeys bits
    verboseCheckWith stdArgs { maxSuccess = 500 } $ prop_AdditiveHomomorphism pub prv
    quickCheckWith stdArgs { maxSuccess = 500 } $ prop_EncrpytDecrypt pub prv

instance Arbitrary PlainText where
    arbitrary = do
        pt <- (arbitrary :: Gen Integer) `suchThat` (>0)
        return $ PlainText pt

prop_EncrpytDecrypt :: PublicKey -> PrivateKey -> PlainText -> Property
prop_EncrpytDecrypt pub prv pt@(PlainText plain) = monadicIO $ do
    c <- run $ encrypt pt pub
    let (PlainText p) = decrypt c prv pub
    assert $ plain == p

prop_AdditiveHomomorphism :: PublicKey -> PrivateKey -> PlainText -> PlainText -> Property
prop_AdditiveHomomorphism pub prv pt1@(PlainText plain1) pt2@(PlainText plain2) =
    monadicIO $ do
        c <- run $ encrypt pt1 pub
        c' <- run $ encrypt pt2 pub
        let c'' = c * c'
        let (PlainText p) = decrypt c'' prv pub
        assert $ p == (plain1 + plain2) 
