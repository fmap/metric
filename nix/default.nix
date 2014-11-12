{ paths ? import ./paths.nix { inherit (import <nixpkgs> {}) fetchgit; }
, haskellPackages ? (import "${paths.nixpkgs}" {}).haskellPackages
, src ? (with builtins; filterSource (path: _: !(elem (baseNameOf path) [".git" "nix"])) ../.)
}: haskellPackages.buildLocalCabal src "metric"
