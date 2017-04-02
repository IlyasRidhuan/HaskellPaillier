### Paillier Cryptosystem

####TODO
- [x] Homomorphic Addition
- [ ] Homomorphic Multiplication
- [ ] Homomorphic Subtraction
- [ ] Negative Number Support

A **[Paillier](https://en.wikipedia.org/wiki/Paillier_cryptosystem)** implementation in Haskell after updating and fixing **[this](https://github.com/onemouth/HsPaillier)**  implementation.

#### Description
The paillier scheme supports binary operations on encrypted data such that when the result is decrypted it equals a binary operation on the unencrypted data.

Multiplication of ciphertext results in the addition of underlying plaintext when the resulting ciphertext is decrypted.
Similarly, exponentiation of ciphertexts results in multiplication of the underlying plaintext.

### Implementation
- Public Key Params :
    * n = p*q where p & q are large primes such that gcd(pq,(p-1)(q-1)). This requirement is guaranteed because p & q are of equal size.
    * g = Is the set of integers less than n^2 & relatively prime to n^2. i.e gcd(g,n^2) == 1

- Private Key Params :
    * λ = lcm(p-1,q-1)
    * μ = a^(1) mod n. Where a^(-1) is the multiplicative modular inverse and a = L (g^λ mod n^2) where L(x) = (x-1)/n

- Encryption:
    * cipher = g^plain * r^n mod n^2

- Decryption:
    * plain = L(cipher^λ mod n^2) * μ mod n

### NOTES
Currently only supports additive homomorphism on positive integers. Key pairs are recommended to be at least > 5 bits as part of the [cryptonite](https://hackage.haskell.org/package/cryptonite) package.

QuickCheck test-suite is available under the test directory

```haskell

main = do
    -- Generate a 64Bit public/private key pair
    Right(pub,prv) <- runExceptT $ genKeys 64

    -- Encrypt 10
    ct  <- encrypt (PlainText 10) pub

    --Encrypt 5
    ct' <- encrypt (PlainText 5) pub

    -- CipherText is has instance Num so multiplication is simple
    let ct'' = ct * ct'

    -- Decryption of resulting product will be 15
    let p = decrypt ct'' prv pub
