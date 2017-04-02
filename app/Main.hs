module Main where

import Paillier
import Control.Monad.Except

main :: IO ()
main = do
    Right(pub,prv) <- runExceptT $ genKeys 64
    ct <- encrypt (PlainText 10) pub
    ct' <- encrypt (PlainText 5) pub
    let ct'' = ct * ct'
    let p = decrypt ct'' prv pub

    putStrLn $ "Encrypting 10 :" ++ show ct
    putStrLn $ "Encrypting 5 :" ++ show ct'
    putStrLn $ "Multiplicating ciphers :" ++ show ct''
    putStrLn $ "Decrypting multiplied cipher :" ++ show p
    putStrLn "Value of plaintext addition is 15"
