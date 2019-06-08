library(tsibble)

test_that("na_rle() for empty `x` with default `index_by`", {
  expect_named(na_rle(), c("lengths", "indices"))
  expect_identical(na_rle()$lengths, integer(0))
  expect_equivalent(na_rle()$indices, integer(0))
  expect_identical(na_rle()$indices %@% "interval", new_interval())
})

test_that("na_rle() for empty `x` with custom `index_by`", {
  expect_is(
    na_rle(index_by = yearmonth("2017-01")[0])$indices,
    "yearmonth"
  )
  expect_is(
    na_rle(index_by = yearquarter("2017-01")[0])$indices,
    "yearquarter"
  )
  expect_is(
    na_rle(index_by = yearweek("2017-01")[0])$indices,
    "yearweek"
  )
})

test_that("na_rle() for unsupported `index_by`", {
  expect_error(na_rle(index_by = character()), "doesn't know")
})

test_that("na_rle()` for different sizes of `x` and `index_by`", {
  expect_error(na_rle(x = 1:3, index_by = 1), "not TRUE")
})

test_that("na_rle() for expected results", {
  expect_identical(
    na_rle(c(1, rep(NA, 4), 6:7, NA, 9:10))$lengths,
    c(4L, 1L)
  )
  expect_equivalent(
    na_rle(c(1, rep(NA, 4), 6:7, NA, 9:10))$indices,
    c(2L, 8L)
  )
  expect_identical(
    na_rle(
      c(1, rep(NA, 4), 6:7, NA, 9:10), 
      index_by = as.Date("2017-01-01") + 0:9
    )$lengths,
    c(4L, 1L)
  )
  expect_equivalent(
    na_rle(
      c(1, rep(NA, 4), 6:7, NA, 9:10), 
      index_by = as.Date("2017-01-01") + 0:9
    )$indices,
    c(as.Date("2017-01-02"), as.Date("2017-01-08"))
  )
})

test_that("na_rle() getters", {
  x <- na_rle(c(1, rep(NA, 4), 6:7, NA, 9:10)) 
  expect_identical(na_rle_lengths(x), x$lengths)
  expect_identical(na_rle_indices(x), x$indices)
})
