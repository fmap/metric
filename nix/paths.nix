{ fetchgit }:

{ nixpkgs = fetchgit {
    url    = "git@github.com/nixos/nixpkgs.git";
    rev    = "d3bcc4ac981b3adf55d9047f1f1f8d453d0ad92";
    sha256 = "e647feb84c17f814620870a1a56631380b4730ebbbc09bea5bf3bf0d48e1a299";
  };
}
