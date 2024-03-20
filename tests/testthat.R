# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(PatientProfiles)
packageurl <- "http://cran.r-project.org/src/contrib/Archive/dbplyr/dbplyr_2.4.0.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

test_check("PatientProfiles")
