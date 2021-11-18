#!/usr/bin/env nix-shell

{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name="discrete-event-simulator";
  buildInputs = [
    pkgs.stack
  ];
}
