{
  description = "smart-csv";
  nixConfig = {
    extra-substituters = "https://kronor-open.cachix.org";
    extra-trusted-public-keys = "kronor-open.cachix.org-1:D1shHZh5BRkmM8RB9BaEqBURIgD/n5+u8KFXD1+DbF8=";
  };
  inputs = {
    git-hooks.url = "github:cachix/git-hooks.nix";
    haskell-nix.url = "github:kronor-io/haskell.nix/wip-nixpkgs-ghc";
    haskell-nix.inputs.hackage.follows = "hackage-nix";
    haskell-nix.inputs.nixpkgs.follows = "nixpkgs";
    kronor-haskell-packages.url = "github:kronor-io/kronor-haskell-packages/repo";
    kronor-haskell-packages.flake = false;
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";
    hackage-nix.url = "github:kronor-io/hackage.nix";
    hackage-nix.flake = false;
  };
  outputs =
    { self
    , git-hooks
    , kronor-haskell-packages
    , haskell-nix
    , hackage-nix
    , nixpkgs
    }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
      mergeAttrs = x: y: x // y;

      foldr = op: nul: list:
        let
          len = builtins.length list;
          fold' = n:
            if n == len
            then nul
            else op (builtins.elemAt list n) (fold' (n + 1));
        in
        fold' 0;

      foldAttrs =
        op:
        nul:
        list_of_attrs:
        foldr
          (n: a:
          foldr
            (name: o:
            o // { ${name} = op n.${name} (a.${name} or nul); }
            )
            a
            (builtins.attrNames n)
          )
          { }
          list_of_attrs;
      eachSystem = f: foldAttrs mergeAttrs { }
        (map (s: builtins.mapAttrs (_: v: { ${s} = v; }) (f s)) supportedSystems);

      readFileIfExists = path:
        if builtins.pathExists path
        then builtins.readFile path
        else null;

      githubActionsLib =
        let
          inherit (builtins) attrValues mapAttrs attrNames;
          flatten = list: builtins.foldl' (acc: v: acc ++ v) [ ] list;
        in
        rec {
          githubPlatforms = {
            "x86_64-linux" = "ubuntu-latest";
            "x86_64-darwin" = "macos-13";
            "aarch64-darwin" = "macos-latest";
            "aarch64-linux" = "ubuntu-24.04-arm";
          };

          mkGithubMatrix =
            { checks
            , attrPrefix ? "githubActions.checks"
            , platforms ? githubPlatforms
            }: {
              inherit checks;
              matrix = {
                include = flatten (attrValues (
                  mapAttrs
                    (
                      system: pkgs: builtins.map
                        (attr:
                          {
                            name = attr;
                            inherit system;
                            os =
                              let
                                os = platforms.${system};
                              in
                              if builtins.typeOf os == "list" then os else [ os ];
                            attr = (
                              if attrPrefix != ""
                              then "${attrPrefix}.${system}.\"${attr}\""
                              else "${system}.\"${attr}\""
                            );
                          })
                        (attrNames pkgs)
                    )
                    checks));
              };
            };
        };
    in
    eachSystem
      (system:
      let
        haskellNix = import haskell-nix {
          inherit system;
          sourcesOverride = {
            hackage = hackage-nix;
          };
        };

        pkgs = import nixpkgs (
          {
            inherit system;
            overlays = [ haskellNix.overlay ];
          }
        );
        pristinePkgs = import nixpkgs (
          {
            inherit system;
          }
        );
        compiler-nix-name = "ghc9122";

        extractGhcOptions = pkgs.runCommand "extract-ghc-options"
          {
            buildInputs = [ pkgs.gawk ];
          }
          ''
            gawk -v need_all=1 -f ${./nix/extract-ghc-options.awk} ${./cabal.project} > $out
          '';

        cabalProjectPackages = import extractGhcOptions.out;

        cabalProjectModules = pkgs.lib.lists.forEach (builtins.attrNames cabalProjectPackages) (name: ({ lib, config, ... }:
          {
            packages.${name} = {
              ghcOptions = let opts = cabalProjectPackages.${name}."ghc_options"; in lib.mkIf (opts != [ ]) opts;
              writeHieFiles = lib.mkIf config.packages.${name}.package.isLocal true;
              enableProfiling = lib.mkIf config.packages.${name}.package.isLocal false;
              enableLibraryProfiling = lib.mkIf config.packages.${name}.package.isLocal false;
              profilingDetail = lib.mkIf config.packages.${name}.package.isLocal "default";
              components = lib.mkIf config.packages.${name}.package.isLocal {
                library = {
                  enableStatic = false;
                  setupHaddockFlags =
                    let opts = cabalProjectPackages.${name}."ghc_options";
                    in
                    lib.mkIf (opts != [ ])
                      (lib.lists.map (x: "--haddock-option=--optghc=" + x)
                        (lib.lists.filter (x: ! lib.strings.hasPrefix "+RTS" x) opts));
                };
              };
            };
          }));


        smartCsvProject = pkgs.haskell-nix.project {
          src = ./.;
          inputMap = {
            "https://kronor-io.github.io/kronor-haskell-packages" = kronor-haskell-packages;
          };
          modules = (if system == "x86_64-darwin" || system == "aarch64-darwin" then [ ] else [{
            dontPatchELF = false;
            dontStrip = false;
          }]) ++ [
            { doHaddock = false; }
          ] ++ cabalProjectModules;

          inherit compiler-nix-name;
          cabalProjectFreeze = builtins.readFile ./cabal.project.freeze;
          supportHpack = false;
        };

        shell = smartCsvProject.shellFor {

          withHoogle = false;

          shellHook = ''
            ${self.checks.${system}.pre-commit-check.shellHook}
          '';
        };

      in
      {
        checks = {
          pre-commit-check = git-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              nixpkgs-fmt.enable = true;
              fourmolu.enable = false;
            };
          };
        };
        packages = {
          default = smartCsvProject.smart-csv-runner.components.exes.smart-csv-runner;
          freezeFile = smartCsvProject.plan-nix.freeze;
        };
        devShells.default = shell;
      }
      ) // {
      githubActions = githubActionsLib.mkGithubMatrix {
        checks = (nixpkgs.lib.attrsets.recursiveUpdate self.checks (
          nixpkgs.lib.attrsets.mapAttrs
            (_: pkgs: nixpkgs.lib.attrsets.removeAttrs pkgs [ "freezeFile" ])
            self.packages
        ));
      };
    };
}
