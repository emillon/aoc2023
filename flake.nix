{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        packages.default = pkgs.ocamlPackages.buildDunePackage {
          pname = "aoc2023";
          version = "n/a";
          src = ./.;
          propagatedBuildInputs = with pkgs.ocamlPackages; [
            angstrom
            core_kernel
            diet
            ocamlgraph
            owl
            ppx_jane
          ];
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = [ self.packages.${system}.default ];
          nativeBuildInputs = with pkgs.ocamlPackages; [
            merlin
            ocamlformat_0_26_1
          ];
        };
      });
}
