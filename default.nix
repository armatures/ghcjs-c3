{ mkDerivation, aeson, base, data-default, ghcjs-base, ghcjs-json
, stdenv, text, time, vector
}:
mkDerivation {
  pname = "ghcjs-c3";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base data-default ghcjs-base ghcjs-json text time vector
  ];
  executableHaskellDepends = [ base data-default ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/mbeidler/ghcjs-c3#readme";
  description = "C3 bindings for GHCJS";
  license = stdenv.lib.licenses.bsd3;
}
