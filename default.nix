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

    OneTuple = dontCheck (self.callHackageDirect {
      pkg = "OneTuple";
      ver = "0.3";
      sha256 = "1xs5zmg1dq815gbb35khbj6jp64f7zgk1hfy8pyf7srm22cjd2dz";
    } {});

    aeson = dontCheck (self.callHackageDirect {
      pkg = "aeson";
      ver = "1.5.6.0";
      sha256 = "18yb8j0jvvzp275ylj16hskgxwdy55hljch9bjvpxl25vvslnk1n";
    } {});

    attoparsec = dontCheck (self.callHackageDirect {
      pkg = "attoparsec";
      ver = "0.13.2.5";
      sha256 = "0qwshlgr85mk73mp2j3bnvg2w30gmsqgn13id0baqwylg797hhmi";
    } {});

    attoparsec-iso8601 = doJailbreak (dontCheck super.attoparsec-iso8601);

    ansi-terminal = dontCheck (self.callHackageDirect {
      pkg = "ansi-terminal";
      ver = "0.10.3";
      sha256 = "1aa8lh7pl054kz7i59iym49s8w473nhdqgc3pq16cp5v4358hw5k";
    } {});

    # Don't run a package's test suite
    # beam-postgres = dontCheck super.beam-postgres;

    base16 = doJailbreak (dontCheck super.base16);

    base16-bytestring = dontCheck (self.callHackageDirect {
      pkg = "base16-bytestring";
      ver = "0.1.1.6";
      sha256 = "1b01lmpp42awyxbxalsdi9fkj7jzl8qzkgmcpbjcq7b4rdb4r9m2";
    } {});

    base64-bytestring = dontCheck (self.callHackageDirect {
      pkg = "base64-bytestring";
      ver = "1.0.0.2";
      sha256 = "1xhlkf7wp15g986nx9npkys1gwrg3mj1jwr26s675kr8jdv9rbfj";
    } {});

    chainweb-api = doJailbreak (dontCheck super.chainweb-api);

    direct-sqlite = dontCheck (self.callHackageDirect {
      pkg = "direct-sqlite";
      ver = "2.3.27";
      sha256 = "0w8wj3210h08qlws40qhidkscgsil3635zk83kdlj929rbd8khip";
    } {});

    hashable = doJailbreak (dontCheck (self.callHackageDirect {
      pkg = "hashable";
      ver = "1.2.7.0";
      sha256 = "0m463ffnsn6vyw2zxhw220wh153ylx1wbp6nd7k2xvqjlyyqnasn";
    } {}));

    http2 = dontCheck (self.callHackageDirect {
      pkg = "http2";
      ver = "2.0.3";
      sha256 = "14bqmxla0id956y37fpfx9v6crwxphbfxkl8v8annrs8ngfbhbr7";
    } {});

    kadena-signing-api = doJailbreak super.kadena-signing-api;
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

    mwc-random = self.callHackageDirect {
      pkg = "mwc-random";
      ver = "0.15.0.2";
      sha256 = "1mpill3lwrrhlzq0ccs8wyzsqhy1a2hmva17qxpgsy2zzqxi1nx1";
    } {};

    # neat-interpolation >= 0.4 breaks Chainweb genesis blocks!
    neat-interpolation = dontCheck (self.callHackageDirect {
      pkg = "neat-interpolation";
      ver = "0.5.1.2";
      sha256 = "0lcgjxw690hyswqxaghf7z08mx5694l7kijyrsjd42yxswajlplx";
    } {});

    pact = dontCheck (appendConfigureFlag super.pact "-f-build-tool");

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
    prettyprinter-ansi-terminal = dontCheck (self.callHackageDirect {
      pkg = "prettyprinter-ansi-terminal";
      ver = "1.1.2";
      sha256 = "0lwcqndppw3jc55rlnn6sp76zmjx2yzl21g9jhg27k2rdnjwd7md";
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

    trifecta = doJailbreak (dontCheck (self.callHackageDirect {
      pkg = "trifecta";
      ver = "2.1";
      sha256 = "0hbv8q12rgg4ni679fbx7ac3blzqxj06dw1fyr6ipc8kjpypb049";
    } {}));

    unordered-containers = dontCheck (self.callHackageDirect {
      pkg = "unordered-containers";
      ver = "0.2.15.0";
      sha256 = "101fjg7jsa0mw57clpjwc2vgrdkrnn0vmf4xgagja21ynwwbl2b5";
    } {});

    warp = dontCheck (self.callHackageDirect {
      pkg = "warp";
      ver = "3.3.6";
      sha256 = "044w7ajkqlwnrpzc4zaqy284ac9wsklyby946jgfpqyjbj87985x";
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
