test_that("na_rle_expand()", {
  x <- na_rle(c(1, rep(NA, 4), 6:7, NA, 9:10))
  expected <- tibble(lengths = c(rep.int(4L, 4), 1L), indices = c(2:5, 8L))
  expect_identical(na_rle_expand(x)$lengths, expected$lengths)
  expect_equivalent(na_rle_expand(x)$indices, expected$indices)
})

test_that("na_rle_table()", {
  x <- na_rle(c(1, rep(NA, 4), 6:7, NA, 9:10))
  expected <- tibble(lengths = c(1L, 4L), n = rep(1L, 2), nobs = lengths * n)
  expect_equal(na_rle_table(x), expected)
})

test_that("na_rle_shift()", {
  x <- na_rle(c(1, rep(NA, 4), 6:7, NA, 9:10))
  expected <- list(lengths = c(4L, 1L), indices = c(2L, 8L))
  expect_equivalent(
    na_rle_indices(na_rle_shift(x, n = 2L)),
    expected$indices + 2L
  )
  expect_identical(
    interval2(na_rle_shift(x, n = 2L)),
    x %@% "interval"
  )
  expect_equivalent(
    na_rle_indices(na_rle_shift(x, n = -2L)),
    expected$indices - 2L
  )
  expect_identical(
    na_rle_shift(x, n = -2L) %@% "interval",
    x %@% "interval"
  )
})

test_that("math operations", {
  x <- c(1, rep(NA, 4), 6:7, NA, 9:10)
  actual <- na_rle(x)
  expect_identical(sum(actual), sum(is.na(x)))
  expect_true(mean(actual) != mean(is.na(x)))
  expect_identical(mean(actual), sum(is.na(x)) / 2)
  expect_identical(min(actual), 1L)
  expect_identical(max(actual), 4L)
})

test_that("set operations", {
  x <- c(1, rep(NA, 4), 6:7, NA, 9:10)
  y <- c(1:2, rep(NA, 5), 8:9, NA)
  x_rle <- na_rle(x)
  y_rle <- na_rle(y)
  expect_identical(intersect(x_rle, y_rle), intersect(y_rle, x_rle))
  expect_identical(
    na_rle_expand(intersect(x_rle, y_rle))$indices,
    dplyr::semi_join(
      na_rle_expand(x_rle), 
      na_rle_expand(y_rle),
      by = "indices"
    )$indices
  )
  expect_identical(union(x_rle, y_rle), union(y_rle, x_rle))
  expect_equivalent(
    na_rle_expand(union(x_rle, y_rle))$indices,
    sort(dplyr::full_join(
      na_rle_expand(x_rle), 
      na_rle_expand(y_rle),
      by = c("indices")
    )$indices)
  )
  expect_equivalent(
    na_rle_indices(setdiff(x_rle, y_rle)),
    setdiff(which(is.na(x)), which(is.na(y)))
  )
  expect_equivalent(
    na_rle_indices(setdiff(y_rle, x_rle)),
    setdiff(which(is.na(y)), which(is.na(x)))[-2]
  )
})
