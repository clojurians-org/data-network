{ nixpkgs ? import <nixpkgs> {}} :

with nixpkgs;
let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      rank1dynamic = dontCheck super.rank1dynamic ;
      network = doJailbreak super.network ;
      network-transport-tcp = doJailbreak super.network-transport-tcp ;
      distributed-static = doJailbreak super.distributed-static ;
      distributed-process = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (self.callCabal2nix "distributed-process"
        (pkgs.fetchFromGitHub {
          owner = "haskell-distributed" ;
          repo = "distributed-process" ;
          rev = "660d554f6acd2dba8b605c84e8fa69e45708bc14" ;
          sha256 = "0c71b3nc19zic9xiirkc41znv93f9j9qlf2kn89mjjyh9w7dazsn" ;
      }) {})) ;

    } ;
  } ;
in
  haskellPackages.developPackage {
    root = ./.;

    modifier = drv: haskell.lib.overrideCabal drv (attrs: {
      buildTools = (attrs.buildTools or []) ++ [haskellPackages.cabal-install] ;
    }) ;
  }

