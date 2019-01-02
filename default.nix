{ mkDerivation, array, base, binary, bytestring, cereal, cond
, directory, exceptions, filepath, gray-extended, hdaemonize
, hsyslog, HUnit, MonadRandom, mtl, QuickCheck, random, split
, stdenv, temporary, test-framework, test-framework-hunit
, test-framework-quickcheck2, time, transformers, unix
}:
mkDerivation {
  pname = "creatur";
  version = "5.9.29";
  src = ./.;
  libraryHaskellDepends = [
    array base binary bytestring cereal cond directory exceptions
    filepath gray-extended hdaemonize hsyslog MonadRandom mtl random
    split time transformers unix
  ];
  testHaskellDepends = [
    base cereal directory filepath HUnit MonadRandom mtl QuickCheck
    temporary test-framework test-framework-hunit
    test-framework-quickcheck2
  ];
  homepage = "https://github.com/mhwombat/creatur#readme";
  description = "Framework for artificial life experiments";
  license = stdenv.lib.licenses.bsd3;
}
