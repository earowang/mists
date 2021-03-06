library(tsibble)
library(vctrs)

test_that("na_rle() for empty `x` with default `index_by`", {
  expected <- list(lengths = integer(), indices = integer())
  expect_identical(fields(x <- na_rle()), names(expected))
  expect_identical(field(x, "lengths"), expected$lengths)
  expect_identical(field(x, "indices"), expected$indices)
  expect_identical(x %@% "interval", new_interval())
  expect_identical(na_rle_inverse(x), integer())
})

test_that("na_rle() for empty `x` with custom `index_by`", {
  expect_is(
    field(na_rle(index_by = yearmonth("2017-01")[0]), "indices"),
    "yearmonth"
  )
  expect_is(
    field(na_rle(index_by = yearquarter("2017-01")[0]), "indices"),
    "yearquarter"
  )
  expect_is(
    field(na_rle(index_by = yearweek("2017-01")[0]), "indices"),
    "yearweek"
  )
})

test_that("na_rle() for unsupported `index_by`", {
  expect_error(na_rle(index_by = character()), "doesn't know")
})

test_that("na_rle() for duplicated `index_by`", {
  expect_error(na_rle(x = rep(NA, 2), index_by = rep(1, 2)), "unique")
})

test_that("na_rle()` for different sizes of `x` and `index_by`", {
  expect_error(na_rle(x = 1:3, index_by = 1), "not TRUE")
})

test_that("na_rle() indexed by default positions", {
  x <- na_rle(c(1, rep(NA, 4), 6:7, NA, 9:10))
  expected <- list(lengths = c(4L, 1L), indices = c(2L, 8L))
  expect_identical(field(x, "lengths"), expected$lengths)
  expect_identical(field(x, "indices"), expected$indices)
})

test_that("na_rle() indexed by dates", {
  x <- list(c(1, rep(NA, 4), 6:7, NA, 9:10), as.Date("2017-01-01") + 0:9)
  actual <- na_rle(x[[1]], index_by = x[[2]])
  expected <- list(lengths = c(4L, 1L), indices = c(x[[2]][2], x[[2]][8]))
  expect_identical(field(actual, "lengths"), expected$lengths)
  expect_identical(field(actual, "indices"), expected$indices)
})

test_that("na_rle(): supplied interval isn't class interval", {
  x <- list(c(1, rep(NA, 4), 6:7, NA, 9:10), as.Date("2017-01-01") + 0:9)
  expect_error(
    na_rle(x[[1]], index_by = x[[2]], interval = 1),
    "interval"
  )
})

test_that("na_rle() getters", {
  x <- na_rle(c(1, rep(NA, 4), 6:7, NA, 9:10)) 
  expect_identical(na_rle_lengths(x), field(x, "lengths"))
  expect_identical(na_rle_indices(x), field(x, "indices"))
  expect_identical(na_rle_starts(x), field(x, "indices"))
  expect_identical(na_rle_ends(x), c(5, 8))
})

test_that("na_rle() subsetting", {
  x <- na_rle(c(1, rep(NA, 4), 6:7, NA, 9:10)) 
  expect_length(x[1], 1)
  expect_is(x[1], "rle_na")
  expect_length(x[1:2], 2)
})

test_that("na_rle() inverse", {
  x <- na_rle(c(1, rep(NA, 4), 6:7, NA, 9:10)) 
  expect_identical(na_rle_inverse(x), c(2:5, 8))
})
