# shell.nix
let
  project = import ./default.nix;
  pkgs = import <nixpkgs> { };
in project.shellFor {
  # ALL of these arguments are optional.

  # Builds a Hoogle documentation index of all dependencies,
  # and provides a "hoogle" command to search the index.
  withHoogle = true;

  # Some common tools can be added with the `tools` argument
  tools = {
    # cabal = "latest";
    # hlint = "latest"; # Selects the latest version in the hackage.nix snapshot
    haskell-language-server = "latest";
  };
  # See overlays/tools.nix for more details

  # Some you may need to get some other way.
  buildInputs = with pkgs; [ mesa freeglut glm mesa_glu ];
  LD_LIBRARY_PATH = with pkgs; "${freeglut}/lib";

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  exactDeps = true;
}
