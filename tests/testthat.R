# load packages
library(testthat)
library(wdpar)

# enable parallel testing
Sys.unsetenv("R_TESTS")

# run tests
test_check("wdpar")
