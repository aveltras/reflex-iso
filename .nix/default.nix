{ reflex-platform ? import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "reflex-frp";
  repo = "reflex-platform";
  rev = "beaa34d244176fc2ff8f70642812141eaf3e559b";
  sha256 = "1sfibav817qpvw8j4fm1jx4m2dvb3a5ra8j2ff6hc8w18i9j0ig4";
}){} }:

reflex-platform.project ({ pkgs, ... }: {
  useWarp = true;
  withHoogle = false;
  

  overrides =
    let
      aeson-gadt-th = pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "aeson-gadt-th";
        rev = "4876c5d1a5fd280a48763b245a8f604ce8777576";
        sha256 = "12mww6i7k6vbsjpfaxpav50zysbhbd6hsxg4q9yjls2m46fxs463";
      };
    in 
      (self: super:{
        aeson-gadt-th = self.callCabal2nix "aeson-gadt-th" "${aeson-gadt-th}" {};
      });

  packages = {
    reflex-iso = ./..;
  };

  shells = {
    ghc = ["reflex-iso"];
    ghcjs = ["reflex-iso"];
  };
})