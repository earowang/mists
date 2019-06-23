test_that("abort if not tsibble", {
  x <- tibble(year = 2019, value = NA)
  expect_error(na_polish_measures(x), "`data` requires a tsibble object.")
})

test_that("na_polish_*() for data with one observation", {
  x <- tsibble::as_tsibble(tibble(year = 2019, value = NA), index = year)
  expect_error(na_polish_measures(x), "cutoff")
  expect_error(na_polish_measures(x, cutoff = 1.1), "between 0 and 1.")
  expect_error(na_polish_measures(x[0L, ]), "empty")
  expect_identical(na_polish_measures(x, cutoff = 1), x[, "year"])
  expect_identical(na_polish_key(x, cutoff = 1), x[0L, ])
  expect_identical(na_polish_index(x, cutoff = 1), x[0L, ])
  expect_identical(na_polish_index2(x, cutoff = 1), x[0L, ])
  expect_identical(rowSums(na_polish_metrics(x, x[0L, ])), 5)
})
