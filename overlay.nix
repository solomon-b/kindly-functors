final: prev: {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = prev.lib.composeExtensions (old.overrides or (_: _: { }))
      (hfinal: hprev: {
        kindly-functors = (hfinal.callCabal2nix "kindly-functors" ./. { }).overrideScope (hfinal': hprev': {
          bifunctors = hfinal.bifunctors_5_6_1;
          semigroupoids = hfinal.semigroupoids_6_0_0_1.overrideScope (hfinal': hprev': {
            bifunctors = hfinal.bifunctors_5_6_1;
          });
        });
      });
  });
}
