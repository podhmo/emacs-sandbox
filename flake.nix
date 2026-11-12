# nix build ".#default"
# nix profile add ".#default"

{
  description = "Cross-platform Emacs (Linux: GTK, macOS: NS) + fancy init dir";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, emacs-overlay }:
    let
      # サポートするシステム一覧（必要に応じて aarch64-linux なども追加）
      systems = [ "x86_64-linux" "aarch64-darwin" ];

      forAllSystems = f: nixpkgs.lib.genAttrs systems (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ emacs-overlay.overlays.default ];
          };
        in f pkgs system
      );
    in
    {
      packages = forAllSystems (pkgs: system:
        let
          emacsPkg = if pkgs.stdenv.isDarwin then
            pkgs.emacs30.override {
              withNS = true;           # これが macOS の GUI
              withGTK3 = false;
              withPgtk = false;
              withTreeSitter = true;
            }
          else
            # Linux → GTK3 版（X11 / Wayland）
            pkgs.emacs30.override {
              withGTK3 = true;
              withPgtk = false;        # pgtk を使いたい場合は true に
              withTreeSitter = true;
            };

          epkgs = pkgs.emacsPackagesFor emacsPkg;

          # all elisp deps of fancy/init.el, resolved on the nix side so the
          # init directory needs no elpa/ and no network access
          emacsFancy = epkgs.emacsWithPackages (ep:
            (with ep.melpaPackages; [
              doom-themes
              solaire-mode
              nerd-icons
              doom-modeline
              vertico
              marginalia
              orderless
              nerd-icons-completion
              posframe
              hl-todo
              rainbow-delimiters
              markdown-mode
              mixed-pitch
              nerd-icons-dired
            ]) ++ (with ep.elpaPackages; [
              spacious-padding
              vertico-posframe
            ]));

          # init dir minus generated state; the store path is read-only so
          # init.el redirects writable state when FANCY_NIX is set
          fancySrc = pkgs.lib.cleanSourceWith {
            src = ./fancy;
            name = "fancy-init";
            filter = name: type:
              !(builtins.elem (baseNameOf name)
                [ "elpa" "eln-cache" "shots" "custom.el" "scrot" "xwd" "magick" ]);
          };

          # fontconfig scans <dir>/share/fonts, so this makes the fonts
          # visible to the wrapped emacs without touching the system profile
          fontDirs = pkgs.lib.concatStringsSep ":" (map (f: "${f}/share") [
            pkgs.jetbrains-mono
            pkgs.noto-fonts-cjk-sans
            pkgs.nerd-fonts.symbols-only
          ]);

          fancyEnv = ''
            export FANCY_NIX=1
            export XDG_DATA_DIRS="${fontDirs}''${XDG_DATA_DIRS:+:$XDG_DATA_DIRS}"
          '';

          fancyTools = pkgs.lib.optionals pkgs.stdenv.isLinux [
            pkgs.scrot
            pkgs.xwd
            pkgs.imagemagick
            pkgs.coreutils
            pkgs.git
          ];
        in
        {
          emacs-gtk = emacsPkg;
          default = emacsPkg;

          # nix run .#fancy  → GUI emacs with fancy/ as init dir
          # nix run .#fancy -- shot <scene.el> <out.png> も同梱の fancy-shot で
          fancy = pkgs.symlinkJoin {
            name = "emacs-fancy";
            paths = [
              (pkgs.writeShellApplication {
                name = "emacs-fancy";
                runtimeInputs = [ emacsFancy ];
                text = ''
                  ${fancyEnv}
                  exec emacs --init-dir=${fancySrc} "$@"
                '';
              })
              (pkgs.writeShellApplication {
                name = "fancy-shot";
                runtimeInputs = [ emacsFancy pkgs.bash ] ++ fancyTools;
                text = ''
                  ${fancyEnv}
                  exec ${fancySrc}/bin/shot "$@"
                '';
              })
            ];
          };
        }
      );

      apps = forAllSystems (pkgs: system: {
        fancy = {
          type = "app";
          program = "${self.packages.${system}.fancy}/bin/emacs-fancy";
        };
      });
    };
}