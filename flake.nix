{
  description = "Runix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      haskellPackages = pkgs.haskellPackages;

      builder = import ./builder.nix {pkgs = nixpkgs.legacyPackages.${system};};
      runix = haskellPackages.developPackage {
          name = "runix";
          root = ./.;
        };
    in
    {
      packages.${system} = {
        default = runix;
        runix = runix;
        builder = builder;
      };

      devShells.${system} = {
        default = haskellPackages.shellFor {
          buildInputs = [
            haskellPackages.haskell-language-server
            haskellPackages.polysemy
            pkgs.cabal-install
            pkgs.cabal2nix
          ];
          withHoogle = true;
          packages = p : with p; [
            polysemy
            ];
        };
      };
    };
}
