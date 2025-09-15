{
    description = "flake for fuzzing fnotation";

    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
        fenix = {
            url = "github:nix-community/fenix";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        crane = {
            url = "github:ipetkov/crane";
        };
    };

    outputs =
    {
        self,
        nixpkgs,
        fenix,
        crane,
        ...
    }@inputs:
    let
    # Linux-specific outputs (NixOS configurations and deploy)
    linuxSystem = "x86_64-linux";

    devShellSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
    ];

    nixpkgsFor =
    system:
    import nixpkgs {
        inherit system;
        config.allowUnfree = true;
    };

    rustToolchainFor =
    system:
    inputs.fenix.packages.${system}.fromToolchainFile {
        file = ./rust-toolchain.toml;
        sha256 = "sha256-mEgn8v8xFz241fdSjNB1CxBHwm3aZz0svD9IqZVZeEA=";
    };

    pkgsLinux = nixpkgsFor linuxSystem;
    rustToolchainLinux = rustToolchainFor linuxSystem;

    craneLib = (crane.mkLib pkgsLinux).overrideToolchain rustToolchainLinux;

    cargoArtifacts = craneLib.buildDepsOnly {
        src = craneLib.cleanCargoSource ./.;
        strictDeps = true;
        nativeBuildInputs = [
            pkgsLinux.pkg-config
        ];

        buildInputs = [
            pkgsLinux.openssl
        ];
    };

    # Generate devShells for each system
    devShellForSystem =
    system:
    let
    pkgs = nixpkgsFor system;
    rustToolchain = rustToolchainFor system;
    in
    pkgs.mkShell {
        name = "catcolab-devshell";

        buildInputs =
        with pkgs;
        [
            rustToolchain
            rust-analyzer
            rustfmt
            clippy
            pkg-config
        ];
        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath (with pkgs; [
            stdenv.cc.cc.lib
        ]);
        # macOS-specific environment variables for OpenSSL and pkg-config
    };
    in
    {
        # Create devShells for all supported systems
        devShells = builtins.listToAttrs (
            map (system: {
                name = system;
                value = {
                    default = devShellForSystem system;
                };
            }) devShellSystems
        );
    };
}
