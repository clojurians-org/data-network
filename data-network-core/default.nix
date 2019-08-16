# export NIX_PATH=~/.nix-defexpr/channels
{ nixpkgs ? import <nixos-19.03> {}} :

with nixpkgs;
let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      map-syntax = doJailbreak super.map-syntax ;
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
      labels-json = dontCheck (doJailbreak (self.callCabal2nix "labels-json" ../dep/labels-json {})) ;

      servant = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (self.callCabal2nix "servant"
        (pkgs.fetchFromGitHub {
          owner = "haskell-servant" ;
          repo = "servant" ;
          rev = "d4289931ad69f1233c9f75f230f7bb29650df433" ;
          sha256 = "019w6h695vi83hsncrqvkf9ykg8zidwn4z1aaf2wz48n39hcizwc" ;} + /servant)
        {})) ;
      
      servant-server = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (self.callCabal2nix "servant-server"
        (pkgs.fetchFromGitHub {
          owner = "haskell-servant" ;
          repo = "servant" ;
          rev = "d4289931ad69f1233c9f75f230f7bb29650df433" ;
          sha256 = "019w6h695vi83hsncrqvkf9ykg8zidwn4z1aaf2wz48n39hcizwc" ;} + /servant-server)
        {})) ;
      
      servant-snap = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (self.callCabal2nix "servant-snap"
        (pkgs.fetchFromGitHub {
          owner = "haskell-servant" ;
          repo = "servant-snap" ;
          rev = "5ed901641c99519fa3e0c701cb430dbb8f6f1b5c" ;
          sha256 = "01w5b4syq775d5pq04cbclvq58wgpimqc22pwkq54yhynmvh7rpq" ;})
        {})) ;
      
      minio-hs = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (self.callCabal2nix "minio-hs"
        (pkgs.fetchFromGitHub {
          owner = "minio" ;
          repo = "minio-hs" ;
          rev = "1e6579b02b2992ed74f4df5fa5d24f90ec3a63d5" ;
          sha256 = "1fs055hj46mjvmq1jfs48skclxfv431mnihjaqnmd2qvja23yvmk" ;
        }) {})) ;
      
      hlibssh2 = dontCheck (doJailbreak (
        addBuildDepends 
          (addPkgconfigDepends (self.callCabal2nix "hlibssh2" ../dep/libssh2-hs/hlibssh2 {}) [libssh2] )
          [libssh2]
      )) ;

    } ;
  } ;
in
  haskellPackages.developPackage {
    root = ./.;

    modifier = drv: haskell.lib.overrideCabal drv (attrs: {
      buildTools = (attrs.buildTools or []) ++ [haskellPackages.cabal-install] ;
    }) ;
  }

