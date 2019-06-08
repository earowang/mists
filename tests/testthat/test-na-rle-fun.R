test_that("na_rle_expand()", {
  x <- na_rle(c(1, rep(NA, 4), 6:7, NA, 9:10))
  expect_identical(na_rle_expand(x)$lengths, c(rep.int(4L, 4), 1L))
  expect_equivalent(na_rle_expand(x)$indices, c(2:5, 8L))
  expect_identical(
    na_rle_expand(x)$indices %@% "interval",
    tsibble::new_interval(unit = 1L)
  )
})

test_that("na_rle_table()", {
  x <- na_rle(c(1, rep(NA, 4), 6:7, NA, 9:10))
  expect_equal(
    na_rle_table(x),
    tibble(lengths = c(1L, 4L), n = rep(1L, 2), nobs = lengths * n)
  )
})

test_that("na_rle_shift()", {
  x <- na_rle(c(1, rep(NA, 4), 6:7, NA, 9:10))
  expect_equivalent(
    na_rle_shift(x, n = 2L)$indices,
    c(4L, 10L)
  )
  expect_identical(
    na_rle_shift(x, n = 2L)$indices %@% "interval",
    x$indices %@% "interval"
  )
  expect_equivalent(
    na_rle_shift(x, n = -2L)$indices,
    c(0L, 6L)
  )
  expect_identical(
    na_rle_shift(x, n = -2L)$indices %@% "interval",
    x$indices %@% "interval"
  )
})

test_that("math operations", {
  x <- c(1, rep(NA, 4), 6:7, NA, 9:10)
  expect_identical(sum(na_rle(x)), sum(is.na(x)))
  expect_true(mean(na_rle(x)) != mean(is.na(x)))
  expect_identical(mean(na_rle(x)), sum(is.na(x)) / 2)
  expect_identical(min(na_rle(x)), 1L)
  expect_identical(max(na_rle(x)), 4L)
})

test_that("set operations", {
  x <- c(1, rep(NA, 4), 6:7, NA, 9:10)
  y <- c(1:2, rep(NA, 5), 8:9, NA)
  expect_identical(
    intersect(na_rle(x), na_rle(y)),
    dplyr::intersect(na_rle(y), na_rle(x))
  )
  expect_identical(
    na_rle_expand(intersect(na_rle(x), na_rle(y)))$indices,
    dplyr::semi_join(
      na_rle_expand(na_rle(x)), 
      na_rle_expand(na_rle(y)),
      by = "indices"
    )$indices
  )
  expect_identical(
    union(na_rle(x), na_rle(y)),
    dplyr::union(na_rle(y), na_rle(x))
  )
  expect_equivalent(
    na_rle_expand(union(na_rle(x), na_rle(y)))$indices,
    sort(dplyr::full_join(
      na_rle_expand(na_rle(x)), 
      na_rle_expand(na_rle(y)),
      by = c("indices")
    )$indices)
  )
  expect_equivalent(
    setdiff(na_rle(x), na_rle(y))$indices,
    dplyr::setdiff(which(is.na(x)), which(is.na(y)))
  )
  expect_equivalent(
    setdiff(na_rle(y), na_rle(x))$indices,
    dplyr::setdiff(which(is.na(y)), which(is.na(x)))[-2]
  )
})
