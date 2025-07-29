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

      # Import templates and task-init script
      templatesModule = import ./templates.nix { inherit pkgs; };
    in
    {
      packages.${system} = {
        default = runix;
        runix = runix;
        builder = builder;
        task-init = templatesModule.task-init;
      };

      devShells.${system} = {
        default = haskellPackages.shellFor {
          buildInputs = [
            haskellPackages.haskell-language-server
            haskellPackages.polysemy
            haskellPackages.http-conduit
            haskellPackages.aeson
            pkgs.cabal-install
            pkgs.cabal2nix
          ];
#          shellHook = ''
#            export GHC_PACKAGE_PATH=$NIX_GHC_LIBDIR/package.conf.d
#          '';
          withHoogle = true;
          packages = p : with p; [
            polysemy
            aeson
            http-conduit
            ];
        };
      };

      templates = {
        task = templatesModule.templates.task;
      };
    };
}
