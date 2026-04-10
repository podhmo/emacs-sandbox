# nix build ".#default"
# nix profile add ".#default"

{
  description = "Cross-platform Emacs (Linux: GTK, macOS: NS)";

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
        in
        {
          emacs-gtk = emacsPkg;
          default = emacsPkg;
        }
      );
    };
}