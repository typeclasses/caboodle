{ haskellPackages }:

(haskellPackages.override { overrides = import ./overrides.nix; }).ghcWithPackages (p: [
  p.containers
  p.hashable
  p.hedgehog
  p.optics
  p.unordered-containers
])
