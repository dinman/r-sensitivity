{host ? "127.0.0.1", port ? "3901", iplotAvailable ? true}:

with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "browse-tpca-tsne";
  phases = "buildPhase";
  dontBuild = true;
  buildInputs = [
    R
    rPackages.data_table
    rPackages.dplyr
    rPackages.GGally
    rPackages.ggplot2
    rPackages.kSamples
    rPackages.memo
    rPackages.plotrix
    rPackages.shiny
    rPackages.shinyjs
  ]
  ++
  (
    if iplotAvailable
      then [
        iplot
        rPackages.codetools
        rPackages.jsonlite
        rPackages.Rcpp
      ]
      else []
  );
  shellHook = ''
    Rscript immersive-ad.R ${host} ${port}
    exit
  '';
}
