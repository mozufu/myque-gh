{
  description = "myque-gh - project myque work items into GitHub";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    myque = {
      url = "github:mozufu/myque/b37ce24c6541d7bf3581e890b5df29c782701cd5";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, myque }:
    let
      systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
      src = nixpkgs.lib.cleanSourceWith {
        name = "myque-gh-src";
        src = nixpkgs.lib.cleanSource ./.;
        filter = path: type:
          let base = baseNameOf path;
          in !((type == "directory" && (base == ".direnv" || base == "dist-newstyle"))
            || nixpkgs.lib.hasPrefix "result" base
            || base == "flake.nix"
            || base == "flake.lock");
      };
      hsPkgsFor = pkgs: pkgs.haskellPackages.override (old: {
        overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {}))
          (hfinal: _hprev: { myque = myque.packages.${pkgs.system}.myque; });
      });
      packageFor = pkgs: (hsPkgsFor pkgs).callPackage ./myque-gh.nix { inherit src; git = pkgs.git; };
    in {
      packages = forAllSystems (pkgs:
        let package = packageFor pkgs;
            rawBin = pkgs.haskell.lib.compose.justStaticExecutables package;
            wrapped = pkgs.symlinkJoin {
              name = "myque-gh-bin";
              paths = [ rawBin ];
              nativeBuildInputs = [ pkgs.makeWrapper ];
              postBuild = ''
                wrapProgram "$out/bin/myque-gh" --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.git pkgs.gh ]}
              '';
            };
        in {
          default = package;
          myque-gh = package;
          myque-gh-bin = wrapped;
        });

      apps = forAllSystems (pkgs: rec {
        default = myque-gh;
        myque-gh = { type = "app"; program = "${self.packages.${pkgs.system}.myque-gh-bin}/bin/myque-gh"; };
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

      checks = forAllSystems (pkgs: { myque-gh = packageFor pkgs; });
      formatter = forAllSystems (pkgs: pkgs.nixfmt);
    };
}
