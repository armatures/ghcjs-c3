{ mkDerivation
, aeson
, base
, data-default
, ghcjs-base ? null
, stdenv
, text
, time
, vector
}:
mkDerivation {
  pname = "ghcjs-c3";
  version = "0.0.0.1";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  isLibrary = true;
  libraryHaskellDepends = [
    aeson base data-default ghcjs-base text time vector
  ];
  homepage = "https://github.com/mbeidler/ghcjs-c3#readme";
  description = "C3 bindings for GHCJS";
  license = stdenv.lib.licenses.bsd3;
}
