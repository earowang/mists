library(dplyr, warn.conflicts = FALSE)
df <- tibble(year = 2000:2019, temp = sample(0:30, size = 20))
df[c(1, 6, 13:16, 19), "temp"] <- NA
df <- df %>%
  mutate(group = rep(letters[1:2], each = 10)) %>%
  group_by(group)

test_that("list_of_na_rle()", {
  expect_is(
    actual <- summarise(df, na_runs = list_of_na_rle(temp, year))$na_runs,
    "list_of_rle_na"
  )
  expect_length(actual, 2)
  expect_identical(actual[[1]], na_rle(df$temp[1:10], df$year[1:10]))
  expect_identical(actual[[2]], na_rle(df$temp[11:20], df$year[11:20]))
})

test_that("list_of_na_rle() math operations", {
  actual <- summarise(df, na_runs = list_of_na_rle(temp, year))$na_runs
  expect_is(range(actual), "vctrs_list_of")
  expect_equal(sum(actual), c(2L, 5L))
  expect_equal(mean(actual), c(2 / 2, 5 / 2))
})

test_that("list_of_na_rle() getters", {
  actual <- summarise(df, na_runs = list_of_na_rle(temp, year))$na_runs
  expect_is(na_rle_indices(actual), "list")
  expect_length(na_rle_indices(actual), 2)
})

test_that("as.list() for list_of_rle_na", {
  actual <- na_rle(c(1, rep(NA, 4), 6:7, NA, 9:10)) 
  expect_is(as.list(actual), "list_of_rle_na")
  expect_identical(
    list_of_na_rle(c(1, rep(NA, 4), 6:7, NA, 9:10)),
    as.list(actual)
  )
})

