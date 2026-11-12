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
          # latest stable emacs (31.1) for all outputs
          emacsPkg = if pkgs.stdenv.hostPlatform.isDarwin then
            pkgs.emacs31.override {
              withNS = true;           # これが macOS の GUI
              withGTK3 = false;
              withPgtk = false;
              withTreeSitter = true;
            }
          else
            # Linux → GTK3 版（X11 / Wayland）
            pkgs.emacs31.override {
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
              which-key-posframe
              ligature
              page-break-lines
              minions
              dashboard
              zoom
            ]) ++ (with ep.elpaPackages; [
              spacious-padding
              vertico-posframe
              diff-hl
              indent-bars
              corfu
              kind-icon
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

          fancyTools = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
            pkgs.scrot
            pkgs.xwd
            pkgs.imagemagick
            pkgs.coreutils
            pkgs.git
          ];

          # macOS (NS) resolves fonts through CoreText and ignores fontconfig /
          # XDG_DATA_DIRS, so the fonts have to live in ~/Library/Fonts.
          # Explicit opt-in command; CJK uses the system Hiragino Sans instead
          # of the (huge) Noto CJK.
          fancyMacFonts = [ pkgs.jetbrains-mono pkgs.nerd-fonts.symbols-only ];
          fancyInstallFonts = pkgs.writeShellApplication {
            name = "fancy-install-fonts";
            text = ''
              dest="$HOME/Library/Fonts/emacs-fancy"
              if [ "''${1:-}" = "--uninstall" ]; then
                rm -rf "$dest" && echo "removed $dest"
                exit 0
              fi
              mkdir -p "$dest"
              # copy, not symlink: store paths can be garbage collected
              for d in ${pkgs.lib.concatStringsSep " " (map (f: "${f}/share/fonts") fancyMacFonts)}; do
                find "$d" -type f -name '*.ttf' -exec cp -f {} "$dest/" \;
              done
              chmod u+w "$dest"/*
              echo "installed into $dest (restart emacs to pick them up)"
            '';
          };
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
                  export FANCY_TOOLS=${./tools}
                  exec ${fancySrc}/bin/shot "$@"
                '';
              })
            ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin [
              fancyInstallFonts
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