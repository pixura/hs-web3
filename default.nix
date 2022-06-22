{ mkDerivation, aeson, async, base, basement, bytestring, cereal
, cryptonite, data-default, exceptions, generics-sop, hpack, hspec
, hspec-contrib, hspec-discover, hspec-expectations, http-client
, http-client-tls, lib, machines, memory, microlens
, microlens-aeson, microlens-mtl, microlens-th, mtl, OneTuple
, parsec, random, split, stm, tagged, template-haskell
, text, time, transformers, uuid-types, vinyl
}:
mkDerivation {
  pname = "web3";
  version = "0.8.3.2";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base basement bytestring cereal cryptonite data-default
    exceptions generics-sop http-client http-client-tls machines memory
    microlens microlens-aeson microlens-mtl microlens-th mtl OneTuple
    parsec tagged template-haskell text transformers uuid-types
    vinyl
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson async base basement bytestring cereal cryptonite data-default
    exceptions generics-sop hspec hspec-contrib hspec-discover
    hspec-expectations http-client http-client-tls machines memory
    microlens microlens-aeson microlens-mtl microlens-th mtl OneTuple
    parsec random split stm tagged template-haskell text time
    transformers uuid-types vinyl
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  homepage = "https://github.com/airalab/hs-web3#readme";
  description = "Ethereum API for Haskell";
  license = lib.licenses.bsd3;
}
