{
  description = "Dev environment for AI 100 Quarto website";

  nixConfig = {
    extra-substituters = [
      "https://rstats-on-nix.cachix.org"
    ];
    extra-trusted-public-keys = [
      "rstats-on-nix.cachix.org-1:vdiiVgocg6WeJrODIqdprZRUrhi1JzhBnXv7aWI6+F0="
    ];
  };

  inputs = {
    nixpkgs.url = "github:rstats-on-nix/nixpkgs/2026-05-11";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    inputs@{
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      perSystem =
        {
          pkgs,
          ...
        }:
        let
          systemPackages = builtins.attrValues {
            inherit (pkgs)
              quarto
              R
              ;
          };
          rLibrary = builtins.attrValues {
            inherit (pkgs.rPackages)
              brand_yml
              dplyr
              fontawesome
              forcats
              fs
              glue
              gt
              here
              knitr
              languageserver
              lubridate
              purrr
              qpdf
              quarto
              readr
              rix
              rmarkdown
              stringr
              tibble
              tidyr
              tidyselect
              ;
          };
        in
        {
          devShells.default = pkgs.mkShell {
            nativeBuildInputs = systemPackages ++ rLibrary;
          };
        };
    };
}
