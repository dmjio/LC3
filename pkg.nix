{ mkDerivation, base, bytestring, hspec, lens, mtl, stdenv, vector
}:
mkDerivation {
  pname = "LC3";
  version = "0.1.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring hspec lens mtl vector
  ];
  description = "LC-3 virtual machine";
  license = stdenv.lib.licenses.bsd3;
}
