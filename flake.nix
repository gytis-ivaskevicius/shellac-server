{
  inputs = {
    # Rust projects support. Custom version of mozilla toolchain can be easily added
    naersk.url = "github:nmattia/naersk";

    # Fancy '$ nix develop'
    devshell.url = "github:numtide/devshell";

    # Few utils to iterate over supported systems.
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils, naersk, devshell }:
    utils.lib.eachDefaultSystem (system:
      let
        inherit (naersk.lib.${system}) buildPackage;
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ devshell.overlay ];
        };
      in
      {

        # nix build
        defaultPackage = buildPackage {
          pname = "shellac-server";
          nativeBuildInputs = with pkgs; [ capnproto ];
          copySources = [ "members" ];
          root = ./.;
        };


        # nix develop
        devShell = pkgs.devshell.mkShell {
          name = "shellac-server";

          # Custom scripts. Also easy to use them in CI/CD
          commands = [
            {
              name = "fmt";
              help = "Check Nix formatting";
              command = "nixpkgs-fmt \${@} $DEVSHELL_ROOT";
            }
          ];

          packages = with pkgs;[ nixpkgs-fmt capnproto rustc cargo stdenv.cc ];
        };
      });
}
