library(testthat)
library(ARGOS)
# testthat::test_check("ARGOS")
devtools::test("ARGOS")

test_files <- testthat::test_dir("tests/testthat/")
print(test_files)
