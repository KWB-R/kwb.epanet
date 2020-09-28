### How to build an R package from scratch
library(usethis) ## or library(devtools)
use_git_config(user.name = "mrustl", user.email = "michael.rustler@kompetenz-wasser.de")

## check by running a git situation-report: 
##   - your user.name and user.email should appear under "User"
git_sitrep()

author <- list(name = "Hauke Sonnenberg",
               email = "hauke.sonnenberg@kompetenz-wasser.de",
               orcid = "0000-0001-9134-2871",
               url = "https://github.com/hsonne")

pkg <- list(name = "kwb.epanet",
            title = "R Package for Interfacing EPANET",
            desc  = paste0("Functions enabling the reading and writing of ", 
                           "EPANET (http://www.epa.gov/nrmrl/wswrd/dw/epanet.html) ",
                           "input files and reading of output files."))

usethis::create_package(path = file.path("..", pkg$name))
fs::file_delete(path = "DESCRIPTION")

kwb.pkgbuild::use_pkg(author,
                      pkg,
                      version = "0.2.0",
                      stage = "maturing")


pkg_dependencies <- c('kwb.utils', 'kwb.plot', 'lattice', 'plot3D', 'gtools')

sapply(pkg_dependencies, usethis::use_package)

usethis::use_vignette("tutorial", "Tutorial")

kwb.pkgbuild::use_autopkgdown()

