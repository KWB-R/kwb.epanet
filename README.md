[![R-CMD-check](https://github.com/KWB-R/kwb.epanet/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.epanet/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.epanet/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.epanet/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.epanet/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.epanet)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.epanet)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.epanet)](https://kwb-r.r-universe.dev/)

# kwb.epanet

Functions enabling the reading and writing of
EPANET (http://www.epa.gov/nrmrl/wswrd/dw/epanet.html) input files and
reading of output files.

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.epanet' from GitHub
remotes::install_github("KWB-R/kwb.epanet")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.epanet](https://kwb-r.github.io/kwb.epanet)

Development: [https://kwb-r.github.io/kwb.epanet/dev](https://kwb-r.github.io/kwb.epanet/dev)
