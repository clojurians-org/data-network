# nix-channel --add https://nixos.org/channels/nixos-19.03
# export NIX_PATH=~/.nix-defexpr/channels
{ nixpkgs ? import <nixos-19.03> {}} :

with nixpkgs;
let
  cleanSource = builtins.filterSource (name: _: let baseName = builtins.baseNameOf name; in !(
    builtins.match "^\\.ghc\\.environment.*" baseName != null ||
    baseName == "cabal.project.local"
  ));

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
      
      hlibssh2 = overrideCabal (self.callCabal2nix "hlibssh2" (cleanSource ../dep/libssh2-hs/hlibssh2) {}) (drv : {
        buildDepends = [ libssh2 ] ;
        pkgconfigDepends = [ libssh2 ] ;
        doCheck = false ;
        jailbreak = true ;
      }) ;

      data-network-core = overrideCabal (self.callCabal2nix "data-network-core" (cleanSource ../data-network-core) {})(drv :{
        doCheck = false ;
      }) ;
      cron = dontCheck super.cron ;
      vinyl = overrideCabal (
        self.callCabal2nix "vinyl" (pkgs.fetchFromGitHub {
            owner = "VinylRecords" ;
            repo = "Vinyl" ;
            rev = "81d6e33f86e5bc621b89e30caad8a76b23742ef4" ;
            sha256 = "1ynjv2lsz30pvl9ki9fb5yprb5p7wwvs7g6sfj03s0ra171m0grn" ;
        }) {}
      ) (drv : {
          doCheck = false ;
          jailbreak = true ;
      }) ;
      vinyl-generics = overrideCabal (
        self.callCabal2nix "vinyl" (pkgs.fetchFromGitHub {
            owner = "VinylRecords" ;
            repo = "Vinyl-generics" ;
            rev = "27e0054a0aeb804e6c8ad3d635cc1815a91cd57c" ;
            sha256 = "1fcfc72s61d5j3r3clz3k54pr3vxwg1k895bamaq6szqscp4324c" ;
        }) {}
      ) (drv : {
          doCheck = false ;
          jailbreak = true ;
      }) ;

    } ;
  } ;
in
  haskellPackages.developPackage {
    root = ./.;

    modifier = drv: haskell.lib.overrideCabal drv (attrs: {
      buildTools = (attrs.buildTools or []) ++ [haskellPackages.cabal-install] ;
    }) ;
  }

