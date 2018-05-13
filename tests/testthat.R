library(testthat)
library(modelplotr)

test_check("modelplotr")

expect_equal(10, 10 + 1e-7)
expect_identical(10, 10 + 1e-7)
