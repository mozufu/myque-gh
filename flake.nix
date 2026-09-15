{
  description = "myque-gh - project myque work items into GitHub";

  inputs = {
    # 26.05 is the final nixpkgs release supporting x86_64-darwin.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    myque = {
      url = "github:mozufu/myque/d25241fcbf1d6b1e06283717c246576e88f6fa5d";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      myque,
    }:
    let
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
      src = nixpkgs.lib.cleanSourceWith {
        name = "myque-gh-src";
        src = nixpkgs.lib.cleanSource ./.;
        filter =
          path: type:
          let
            base = baseNameOf path;
          in
          !(
            (type == "directory" && (base == ".direnv" || base == "dist-newstyle"))
            || nixpkgs.lib.hasPrefix "result" base
            || base == "flake.nix"
            || base == "flake.lock"
          );
      };
      hsPkgsFor =
        pkgs:
        pkgs.haskellPackages.override (old: {
          overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { })) (
            hfinal: _hprev: { myque = myque.packages.${pkgs.stdenv.hostPlatform.system}.myque; }
          );
        });
      packageFor =
        pkgs:
        (hsPkgsFor pkgs).callPackage ./myque-gh.nix {
          inherit src;
          git = pkgs.git;
        };
    in
    {
      packages = forAllSystems (
        pkgs:
        let
          package = packageFor pkgs;
          rawBin = pkgs.haskell.lib.compose.justStaticExecutables package;
          wrapped = pkgs.symlinkJoin {
            name = "myque-gh-bin";
            paths = [ rawBin ];
            nativeBuildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram "$out/bin/myque-gh" --prefix PATH : ${
                pkgs.lib.makeBinPath [
                  pkgs.git
                  pkgs.gh
                ]
              }
            '';
          };
          hostedSmokeRaw = pkgs.haskell.lib.compose.justStaticExecutables (
            pkgs.haskell.lib.compose.dontCheck (
              pkgs.haskell.lib.compose.setBuildTarget "myque-gh-hosted-smoke" package
            )
          );
          hostedSmoke = pkgs.symlinkJoin {
            name = "myque-gh-hosted-smoke-bin";
            paths = [
              hostedSmokeRaw
              wrapped
            ];
            nativeBuildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram "$out/bin/myque-gh-hosted-smoke" --prefix PATH : ${
                pkgs.lib.makeBinPath [
                  pkgs.git
                  pkgs.gh
                  wrapped
                ]
              }
            '';
          };
        in
        {
          default = package;
          myque-gh = package;
          myque-gh-bin = wrapped;
          myque-gh-hosted-smoke = hostedSmoke;
        }
      );

      apps = forAllSystems (pkgs: rec {
        default = myque-gh;
        myque-gh = {
          type = "app";
          program = "${self.packages.${pkgs.stdenv.hostPlatform.system}.myque-gh-bin}/bin/myque-gh";
        };
        myque-gh-hosted-smoke = {
          type = "app";
          program = "${
            self.packages.${pkgs.stdenv.hostPlatform.system}.myque-gh-hosted-smoke
          }/bin/myque-gh-hosted-smoke";
        };
      });

      devShells = forAllSystems (pkgs: {
        default = (hsPkgsFor pkgs).shellFor {
          name = "myque-gh-shell";
          packages = _: [ (packageFor pkgs) ];
          nativeBuildInputs = [
            pkgs.cabal-install
            (hsPkgsFor pkgs).cabal2nix
            (hsPkgsFor pkgs).fourmolu
            pkgs.git
            pkgs.gh
          ];
        };
      });

      checks = forAllSystems (pkgs: {
        myque-gh = packageFor pkgs;
      });
      formatter = forAllSystems (pkgs: pkgs.nixfmt);
    };
}
