[![Appveyor build Status](https://ci.appveyor.com/api/projects/status/github/KWB-R/kwb.epanet?branch=master&svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-epanet/branch/master)
[![Travis build Status](https://travis-ci.org/KWB-R/kwb.epanet.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.epanet)
[![codecov](https://codecov.io/github/KWB-R/kwb.epanet/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.epanet)
[![Project Status](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.epanet)]()

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
