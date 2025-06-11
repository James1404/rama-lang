{
  description = "A custom lisp dialect";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, flake-utils, nixpkgs, rust-overlay, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [(import rust-overlay)];  };
        in {
          devShells.default = (pkgs.mkShell.override { stdenv = pkgs.clangStdenv; } {
            packages = with pkgs; [
              pkg-config
              lldb
              gdb

              (rust-bin.selectLatestNightlyWith (toolchain: toolchain.default.override {
                extensions = [ "rust-analyzer" "rust-src" ];
              }))

              llvmPackages_20.libllvm

              libxml2

              cargo-udeps
              clippy
              tokei

              aflplusplus
            ];

            RUST_BACKTRACE = "1";
            LLVM_SYS_201_PREFIX = "${pkgs.llvmPackages_20.libllvm.dev}";
          });
        }
      );
}
