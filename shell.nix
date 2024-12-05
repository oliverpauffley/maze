{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  name = "Mazes";
  buildInputs = [ mesa freeglut glm mesa_glu ];
  LD_LIBRARY_PATH = with pkgs; "${freeglut}/lib";
}
