{
  inputs.flakelight.url = "github:nix-community/flakelight";
  outputs = { flakelight, ... }:
    flakelight ./. {
      systems = [ "aarch64-darwin" ];
      devShell = {
        shellHook = ''
          if [ ! -f .bsp/scala-cli.json ]; then
            scala-cli setup-ide . -language:strictEquality
          fi
        '';
        packages = pkgs: with pkgs; [
          scala-cli
          metals
        ];
      };
    };
}
