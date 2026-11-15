{
  description = "shinycoro: Persistent coroutines for shiny";

  inputs = {
    nullkomma.url = "https://flakehub.com/f/dataheld/nullkomma/0.1.*";
    nixpkgs.follows = "nullkomma/nixpkgs";
    maxheld = {
      url = "https://flakehub.com/f/dataheld/maxheld/*";
      inputs.nullkomma.follows = "nullkomma";
    };
  };

  outputs =
    inputs:
    inputs.nullkomma.lib.mkFlake { inherit inputs; } {
      nullkomma.brand = inputs.maxheld;
      nullkomma.cloudflare = {
        enable = true;
        domain = "maxheld.de";
        name = "shinycoro";
      };
      nullkomma.r.enable = true;
      nullkomma.r.shiny = {
        enable = true;
        function = "hello";
      };
      nullkomma.r.pkgdown.brand.enable = true;
      nullkomma.r.hashes.rdtools = "sha256-8BQ/3Vdl1vndn4BNdoHfjo4TP4ioBWv3Y1bZu22ej70=";
    };
}
