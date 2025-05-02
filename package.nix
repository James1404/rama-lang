{
  lib,
  fetchFromGithub,
  rustPlatform,
  llvmPackages_20,
}:

rustPlatform.buildRustPackage (finalAttrs: {
  pname = "rama";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [
    llvmPackages_20.libllmv
  ];

  LLVM_SYS_201_PREFIX = "${llvmPackages_20.libllvm.dev}";

  useFetchCargoVendor = true;
  cargoHash = "sha256-s6LCwODDyze4HbG/Jyz3vu2t4DAkhTmdL7ra2vGYcew=";

  meta = {
    description = "The Rama compiler";
    homepage = "https://github.com/James1404/rama-lang";
    license = lib.licenses.mit;
    maintainers = [ "James Barnfather" ];
  };
})
