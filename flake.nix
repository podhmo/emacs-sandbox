# nix build ".#emacs-gtk"
# nix profile install ".#emacs-gtk"

{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, emacs-overlay }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ emacs-overlay.overlays.default ];
      };
    in {
      packages.${system}.emacs-gtk = pkgs.emacs30.override {
        withGTK3 = true;
        withPgtk = false;   # pgtk無効 → gtk版（X11）
        withTreeSitter = true;
      };

      default  = self.packages.${system}.emacs-gtk;
    };
}
