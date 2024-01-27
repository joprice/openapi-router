{
  description = "openapi-router";

  inputs = {
    nix-filter.url = "github:numtide/nix-filter";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs = {
      url = "github:nix-ocaml/nix-overlays";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}".appendOverlays [
          (self: super: { ocamlPackages = super.ocaml-ng.ocamlPackages_5_1; })
        ];
      in

      rec {
        packages = with pkgs.ocamlPackages; {
          openapi-router = buildDunePackage {
            pname = "openapi_router";
            version = "n/a";
            src = with nix-filter.lib; filter {
              root = ./.;
              include = [
                "dune-project"
                "dune"
                "openapi_router.opam"
                "lib"
              ];
            };
            propagatedBuildInputs = [
              core
              yojson
              ppx_deriving
              ppx_yojson_conv
            ];
          };
        } // { default = packages.openapi-router; };

        devShells = {
          default = pkgs.mkShell {
            dontDetectOcamlConflicts = true;
            inputsFrom = pkgs.lib.attrValues packages;
            nativeBuildInputs = with pkgs.ocamlPackages; [
              ocamlformat
            ];
            propagatedBuildInputs = with pkgs.ocamlPackages; [
              merlin
            ];
          };
        };
      });
}
