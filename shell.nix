let
  defaultPkgs = import <nixpkgs> {};
  reflex-platform-drv = defaultPkgs.fetchgit {
    url = "https://github.com/ConferHealth/reflex-platform";
    rev = "3a6e0e7084deae03add08a9dab50b67847d63963";
    sha256 = "0lvyw4z1rifl6ccagmn0173cks9dlbm06nqasdl6sqdhpj60jkgy";
  };

  reflex-platform = import reflex-platform-drv {};
  compiler = reflex-platform.ghcjs;
in
  reflex-platform.workOn compiler (compiler.callPackage ./default.nix { })
