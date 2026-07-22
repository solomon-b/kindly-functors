final: prev: {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = prev.lib.composeExtensions (old.overrides or (_: _: { }))
      (hfinal: hprev: {
        kindly-functors = hfinal.callCabal2nix "kindly-functors" ./. { };
      });
  });
}
