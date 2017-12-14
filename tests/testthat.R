# load packages
library(testthat)
library(wdpa)

# enable parallel testing
Sys.unsetenv("R_TESTS")

# run tests
test_check("wdpa")
