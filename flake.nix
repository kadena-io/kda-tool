{
  inputs.hs-nix-infra.url = "github:kadena-io/hs-nix-infra";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nix-exe-bundle = { url = "github:3noch/nix-bundle-exe"; flake = false; };
  outputs = inputs@{ self, flake-utils, hs-nix-infra, ...}:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ] (system:
    let
      inherit (hs-nix-infra) haskellNix nixpkgs;
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          kdaToolProject =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              shell.tools = {
                cabal = {};
              };
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
              ];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      project = pkgs.kdaToolProject;
      flake = project.flake {
        # This adds support for `nix build .#js-unknown-ghcjs:hello:exe:hello`
        # crossPlatforms = p: [p.ghcjs];
      };
      # This package depends on other packages at buildtime, but its output does not
      # depend on them. This way, we don't have to download the entire closure to verify
      # that those packages build.
      mkCheck = name: package: pkgs.runCommand ("check-"+name) {} ''
        echo ${name}: ${package}
        echo works > $out
      '';
      default = flake.packages."kda-tool:exe:kda";
      bundled = (pkgs.callPackage inputs.nix-exe-bundle {} default).overrideAttrs
        (_: {inherit (default) version;});
    in {
      # Built by `nix build .`
      packages = rec {
        inherit default bundled;
        recursive = with hs-nix-infra.lib.recursive system;
          wrapRecursiveWithMeta "kda" "${wrapFlake self}.default";

        check = pkgs.runCommand "check" {} ''
          echo ${default}
          echo ${mkCheck "devShell" flake.devShell}
          echo works > $out
        '';
      };

      inherit (flake) devShell;

      # Other flake outputs provided by haskellNix can be accessed through
      # this project output
      inherit project;
    });
}
