test_that("NA helpers for correctness", {
  x <- c(rep(NA, 4), 10:6, NA, NA, 4:1, NA)
  expect_identical(na_starts_with(x), 4L)
  expect_identical(na_starts_with(x[5:length(x)]), 0L)
  expect_identical(na_ends_with(x), 1L)
  expect_identical(na_ends_with(x[-length(x)]), 0L)
  expect_identical(na_elsewhere(x), 2L)
  expect_identical(na_elsewhere(x[5:(length(x) - 1)]), 2L)
})

test_that("NA helpers for empty input", {
  x <- integer()
  expected <- length(is.na(x))
  expect_identical(na_starts_with(x), expected)
  expect_identical(na_ends_with(x), expected)
  expect_identical(na_elsewhere(x), expected)
})
