{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "A Hello World in Haskell with a dependency and a devShell";
  inputs.nixpkgs.url = "nixpkgs";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        haskell-hello = final.haskell.packages.ghc923.callPackage (import ./default.nix) {} ;
      });
      packages = forAllSystems (system: {
         haskell-hello = nixpkgsFor.${system}.haskell-hello;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.haskell-hello);
      checks = self.packages;
      devShell = forAllSystems (system: let haskellPackages = nixpkgsFor.${system}.haskell.packages.ghc923;
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.haskell-hello];
          buildInputs = with haskellPackages;
            let
              pkgs = nixpkgsFor.${system};
            in
              [
                haskell-language-server
                cabal-install
                pkgs.zlib
              ];
        # Change the prompt to show that you are in a devShell
        # shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
        });
  };
}
