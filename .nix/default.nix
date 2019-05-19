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
      # constraints-extras = pkgs.fetchFromGitHub {
      #   owner = "obsidiansystems";
      #   repo = "constraints-extras";
      #   rev = "30f10c03dd96e50c089f0613f99951805bff7397";
      #   sha256 = "196b8kbcp744gqhh964m54vw4cdg15p6lc7cm2vxbh15cbqdz7ir";
      # };
      # reflex-dom = pkgs.fetchFromGitHub {
      #   owner = "reflex-frp";
      #   repo = "reflex-dom";
      #   rev = "54d1291e36b6ff609a8114c24c66c32c4f664acf";
      #   sha256 = "1cr1k2jvqy99970lh3rym28zh6n5442sy85kmzss7m9wiavcxymh";
      # };
    in 
      (self: super:{
        # silently = pkgs.haskell.lib.dontCheck super.silently;
        # markdown-unlit = pkgs.haskell.lib.dontCheck super.markdown-unlit;
        # aeson-gadt-th = pkgs.haskell.lib.dontCheck (self.callCabal2nix "aeson-gadt-th" "${aeson-gadt-th}" {});
        aeson-gadt-th = self.callCabal2nix "aeson-gadt-th" "${aeson-gadt-th}" {};
        # constraints-extras = self.callCabal2nix "constraints-extras" "${constraints-extras}" {};
        # reflex-dom = self.callCabal2nix "reflex-dom" "${reflex-dom}/reflex-dom" {};
        # reflex-dom-core = self.callCabal2nix "reflex-dom-core" "${reflex-dom}/reflex-dom-core" {};
      });

  packages = {
    reflex-iso = ./..;
  };

  shells = {
    ghc = ["reflex-iso"];
    ghcjs = ["reflex-iso"];
  };
})