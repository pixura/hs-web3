-- |
-- Module      :  Crypto.Ethereum.Utils
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- EC cryptography on Secp256k1 curve.
--

module Crypto.Ethereum.Utils where

import           Crypto.Hash                (Keccak_256 (..), hashWith)
import           Crypto.Number.Serialize    (i2osp, os2ip)
import           Crypto.PubKey.ECC.ECDSA    (PrivateKey (..), PublicKey (..))
import           Crypto.PubKey.ECC.Generate (generateQ)
import           Crypto.PubKey.ECC.Types    (CurveName (SEC_p256k1), Point (..),
                                             getCurveByName)
import           Data.ByteArray             (ByteArray, ByteArrayAccess,
                                             convert)
import           Data.Monoid                ((<>))

-- | Import Ethereum private key from byte array.
--
-- Input array should have 32 byte length.
importKey :: ByteArrayAccess privateKey => privateKey -> PrivateKey
{-# INLINE importKey #-}
importKey = PrivateKey (getCurveByName SEC_p256k1) . os2ip

-- | Export private key to byte array (32 byte length).
exportKey :: ByteArray privateKey => PrivateKey -> privateKey
{-# INLINE exportKey #-}
exportKey (PrivateKey _ key) = i2osp key

-- | Get public key appropriate to private key.
--
-- /WARNING:/ Vulnerable to timing attacks.
derivePubKey :: PrivateKey -> PublicKey
{-# INLINE derivePubKey #-}
derivePubKey (PrivateKey curve p) = PublicKey curve (generateQ curve p)

-- | Export public key to byte array (64 byte length).
exportPubKey :: ByteArray publicKey => PublicKey -> publicKey
{-# INLINE exportPubKey #-}
exportPubKey (PublicKey _ (Point x y)) = i2osp x <> i2osp y
exportPubKey (PublicKey _ PointO)      = mempty

-- | Keccak 256 hash function.
sha3 :: (ByteArrayAccess bin, ByteArray bout) => bin -> bout
{-# INLINE sha3 #-}
sha3 = convert . hashWith Keccak_256
