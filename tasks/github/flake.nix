{
  description = "runix-github";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    runix = {
      type = "path";
      path = "/home/newmind/git/runix/";
    };
  };

  outputs = { self, nixpkgs, runix, ... }:
    let
      system = "x86_64-linux";
      builder = runix.outputs.packages.${system}.builder;
      #builderf = import /home/newmind/git/runix/builder.nix;
      #builder = builderf.buildTask;
      pkgs = nixpkgs.legacyPackages.${system};


      definition = {mkDerivation, runix, aeson, polysemy} : mkDerivation {
        pname = "runix-github";
        src = self; 
        version = "0.1.0.0";
        license = "GPL";
        libraryHaskellDepends = [runix aeson polysemy];
      };

      inherit (pkgs.haskellPackages) aeson polysemy;
      maketrusted = builder.maketrusted;
      result = builder.buildTask definition 
        {
          runix = runix.outputs.packages.${system}.default;
          polysemy = maketrusted polysemy;
          aeson = maketrusted aeson;
        };
    in
    {
      packages.${system} = {
        default = result;
      };
    };
}
