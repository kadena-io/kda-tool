{ compiler ? "ghc881"
, rev      ? "c4f97342ba8ac84def72328616dd05d005bb4715"
, sha256   ? "1p2gbisib2jrz4r9b5vzfvmirgmz9sr2ksalngaw908vvg9hsvai"
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

    # Don't run a package's test suite
    # beam-postgres = dontCheck super.beam-postgres;

    # Get a specific hackage version straight from hackage. Unlike the above
    # callHackage approach, this will always succeed if the version is on
    # hackage. The downside is that you have to specify the hash manually.
    mwc-random = self.callHackageDirect {
      pkg = "mwc-random";
      ver = "0.14.0.0";
      sha256 = "10jaajbnlcwqgqdnb94q0k8pzx11ff569af8a8d6k26xc954m48p";
    } {};
    
    base16 = dontCheck (self.callHackageDirect {
      pkg = "base16";
      ver = "0.3.1.0";
      sha256 = "07p2cxpsklxi11v84d16gnbdf4z4qi6xxa0k6nl7bpxc4rzi2v38";
    } {});

    base64 = dontCheck (self.callHackageDirect {
      pkg = "base64";
      ver = "0.4.2.3";
      sha256 = "1i4cf1xfbkxlxshwlsxgw2w5gi3hkkfm1n99vnzq7rixz8nxcw7r";
    } {});

    direct-sqlite = dontCheck (self.callHackageDirect {
      pkg = "direct-sqlite";
      ver = "2.3.27";
      sha256 = "0w8wj3210h08qlws40qhidkscgsil3635zk83kdlj929rbd8khip";
    } {});

    katip = dontCheck super.katip;

    libBF = doJailbreak (dontCheck (self.callHackageDirect {
      pkg = "libBF";
      ver = "0.6.3";
      sha256 = "0j0i39jb389rnrkkw2xqz10471afxys79nf31hhlqr4fk6ddhjf7";
    } {}));

    megaparsec = dontCheck (self.callHackageDirect {
      pkg = "megaparsec";
      ver = "9.0.0";
      sha256 = "03kqcfpsmi5l4mr6lsmlpks2mp9prf9yy97mmrkclwqpxybdjx2l";
    } {});

    # neat-interpolation >= 0.4 breaks Chainweb genesis blocks!
    neat-interpolation = dontCheck (self.callHackageDirect {
      pkg = "neat-interpolation";
      ver = "0.5.1.2";
      sha256 = "0lcgjxw690hyswqxaghf7z08mx5694l7kijyrsjd42yxswajlplx";
    } {});

    pact = dontCheck super.pact;

    pact-time = dontCheck (self.callHackageDirect {
      pkg = "pact-time";
      ver = "0.2.0.0";
      sha256 = "1cfn74j6dr4279bil9k0n1wff074sdlz6g1haqyyy38wm5mdd7mr";
    } {});

    # prettyprinter > 1.6.0 breaks binary compatibility of Pact payloads
    # inside Chainweb blocks!
    prettyprinter = dontCheck (self.callHackageDirect {
      pkg = "prettyprinter";
      ver = "1.6.0";
      sha256 = "0f8wqaj3cv3yra938afqf62wrvq20yv9jd048miw5zrfavw824aa";
    } {});

    sbv = dontCheck (self.callHackageDirect {
      pkg = "sbv";
      ver = "9.0";
      sha256 = "14g2qax1vc7q4g78fa562dviqvcd0l52kd5jmgv90g3g3ci15bnl";
    } {});

    # comment from pact...
    # sbv requires this even though it is not used in the build (and the hash is invalid)
    tasty-bench = dontCheck (self.callHackageDirect {
      pkg = "tasty-bench";
      ver = "0.3.1";
      sha256 = "0000000000000000000000000000000000000000000000000000";
    } {});

    unordered-containers = dontCheck (self.callHackageDirect {
      pkg = "unordered-containers";
      ver = "0.2.15.0";
      sha256 = "101fjg7jsa0mw57clpjwc2vgrdkrnn0vmf4xgagja21ynwwbl2b5";
    } {});

    # To discover more functions that can be used to modify haskell
    # packages, run "nix-repl", type "pkgs.haskell.lib.", then hit
    # <TAB> to get a tab-completed list of functions.
  };

  source-overrides = {
    HsYAML = nix-thunk.thunkSource ./deps/HsYAML;
    HsYAML-aeson = nix-thunk.thunkSource ./deps/HsYAML-aeson;
    cardano-crypto = nix-thunk.thunkSource ./deps/cardano-crypto;
    chainweb-api = thunkSource ./deps/chainweb-api;
    pact = thunkSource ./deps/pact;
    kadena-signing-api = (thunkSource ./deps/signing-api) + "/kadena-signing-api";
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      hpkgs.cabal-install
      hpkgs.ghcid
    ];
  });
}
