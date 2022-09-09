{ compiler ? "ghc884"
, rev      ? "eded7b8a2387e8c10aa45b951f47a13ca28def38"
, sha256   ? "148njnx1j90q94gyqyp92knhcf2xypy5vy48pd9kaq23adi57p9c"
, pkgs     ?
    import (builtins.fetchTarball {
      url    = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      inherit sha256; }) {
      config.allowBroken = false;
      config.allowUnfree = true;
    }
}:
let gitignoreSrc = import (pkgs.fetchFromGitHub {
      owner = "hercules-ci";
      repo = "gitignore";
      rev = "2ced4519f865341adcb143c5d668f955a2cb997f";
      sha256 = "0fc5bgv9syfcblp23y05kkfnpgh3gssz6vn24frs8dzw39algk2z";
    }) {};
    hpkgs = pkgs.haskell.packages.${compiler};

    # See https://github.com/obsidiansystems/nix-thunk for documentation
    nix-thunk = import ./deps/nix-thunk {};
    inherit (nix-thunk) thunkSource;
in
hpkgs.developPackage {
  name = builtins.baseNameOf ./.;
  root = gitignoreSrc.gitignoreSource ./.;

  overrides = self: super: with pkgs.haskell.lib;
  {
    HsYAML = dontCheck super.HsYAML;
    HsYAML-aeson = dontCheck super.HsYAML-aeson;

    attoparsec = dontCheck (self.callHackageDirect {
      pkg = "attoparsec";
      ver = "0.13.2.5";
      sha256 = "0qwshlgr85mk73mp2j3bnvg2w30gmsqgn13id0baqwylg797hhmi";
    } {});

    attoparsec-iso8601 = doJailbreak (dontCheck super.attoparsec-iso8601);

    # Don't run a package's test suite
    # beam-postgres = dontCheck super.beam-postgres;

    base16 = doJailbreak (dontCheck super.base16);

    chainweb-api = doJailbreak (dontCheck super.chainweb-api);

    direct-sqlite = dontCheck (self.callHackageDirect {
      pkg = "direct-sqlite";
      ver = "2.3.27";
      sha256 = "0w8wj3210h08qlws40qhidkscgsil3635zk83kdlj929rbd8khip";
    } {});

    kadena-signing-api = doJailbreak super.kadena-signing-api;
    katip = dontCheck super.katip;

    pact-time = dontCheck (self.callHackageDirect {
      pkg = "pact-time";
      ver = "0.2.0.0";
      sha256 = "1cfn74j6dr4279bil9k0n1wff074sdlz6g1haqyyy38wm5mdd7mr";
    } {});

    prettyprinter-ansi-terminal = dontCheck (self.callHackageDirect {
      pkg = "prettyprinter-ansi-terminal";
      ver = "1.1.2";
      sha256 = "0lwcqndppw3jc55rlnn6sp76zmjx2yzl21g9jhg27k2rdnjwd7md";
    } {});

    cryptonite = dontCheck (appendConfigureFlag super.cryptonite "-fsupport_pclmuldq");
    hashable       = doJailbreak super.hashable;
    pact = dontCheck (appendConfigureFlag super.pact "-fcryptonite-ed25519 -f-build-tool -fno-advice");


    # These tests pull in unnecessary dependencies
    http2         = dontCheck super.http2;
    prettyprinter = dontCheck super.prettyprinter;
    aeson         = dontCheck super.aeson;
  };

  source-overrides = {
    HsYAML = nix-thunk.thunkSource ./deps/HsYAML;
    HsYAML-aeson = nix-thunk.thunkSource ./deps/HsYAML-aeson;
    cardano-crypto = nix-thunk.thunkSource ./deps/cardano-crypto;
    chainweb-api = thunkSource ./deps/chainweb-api;
    pact = thunkSource ./deps/pact;
    kadena-signing-api = (thunkSource ./deps/signing-api) + "/kadena-signing-api";

    OneTuple                    = "0.3";
    aeson                       = "1.5.6.0";
    ansi-terminal               = "0.11.3";
    prettyprinter-ansi-terminal = "1.1.2";
    time-compat                 = "1.9.5";
    trifecta                    = "2.1.1";
    unordered-containers        = "0.2.15.0";

    # These are required in order to not break payload validation
    base16-bytestring = "0.1.1.7";
    prettyprinter     = "1.6.0";
    hashable          = "1.3.0.0";
    base64-bytestring = "1.0.0.3";
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      hpkgs.cabal-install
      hpkgs.ghcid
    ];
  });
}
