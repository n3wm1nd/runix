{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    aiflow.url = "github:your-username/aiflow"; # Replace with the actual Aiflow repository
    aiflow.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, aiflow, ... }:
    let
      inherit (self) outputs;
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      defaultPackage.${system} =
        aiflow.packages.${system}.aiflow.buildRunner ./. (self.legacyPackages.${system}.tasks or []);

      legacyPackages.${system} = {
        tasks = pkgs.lib.mkOption {
          type = pkgs.lib.types.listOf pkgs.lib.types.package;
          default = [];
          description = "List of tasks to include in the runner";
        };
      };
    };
}
