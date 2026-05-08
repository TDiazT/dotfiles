{ config, pkgs, ...}:
let 
in {
  environment.systemPackages = with pkgs; [
    kitty
  ];

  programs.hyprland = {
    enable = true;
    withUWSM = true;
  };

  programs.uwsm.enable = true;

  fonts.packages = with pkgs; [
    fira-code
    nerd-fonts.jetbrains-mono
  ];
}
