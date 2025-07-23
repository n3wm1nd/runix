{pkgs} :
{
  buildTask = fn: args:
      let
        drv = pkgs.haskellPackages.callPackage fn args;
      in
       pkgs.haskell.lib.compose.overrideCabal (old: {configureFlags = ["--ghc-option=-XSafe"];}) drv;

  maketrusted = pkgs.haskell.lib.compose.overrideCabal (old: {configureFlags = ["--ghc-option=-XTrustworthy"];});
}
