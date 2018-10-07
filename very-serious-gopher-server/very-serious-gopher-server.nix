{ mkDerivation, base, bytestring, network, stdenv, text }:
mkDerivation {
  pname = "very-serious-gopher-server";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring network text ];
  homepage = "https://averyserious.website/";
  description = "A Very Serious GOPHER Server";
  license = stdenv.lib.licenses.gpl3;
}
