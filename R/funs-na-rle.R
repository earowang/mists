globalVariables(c("n", "indices2"))

#' @export
as_tibble.rle_na <- function(x, ...) {
  as_tibble(unclass(x))
}

#' @export
as_tibble.list_of_rle_na <- function(x, ...) {
  tbl <- add_column_id(x, ...)
  new_col <- names(tbl)
  y <- tbl[[new_col]]
  res_lst <-
    map2(x, y, function(.x, .y) mutate(as_tibble(.x), !! new_col := .y))
  vec_rbind(!!! res_lst)
}

#' Shift run length encoding <`NA`>
#'
#' @param x An object returned by [`na_rle()`] or [`list_of_na_rle()`].
#' @param n An integer shifts the position. If positive, shifts to the right,
#' otherwise to the left.
#'
#' @examples
#' (x <- na_rle(c(1, NA, NA, 4:7, NA, NA, 10:15, NA)))
#' na_rle_shift(x, n = 2)
#' na_rle_shift(x, n = -5)
#' @export
na_rle_shift <- function(x, n = 1L) {
  if (n == 0) return(x)
  UseMethod("na_rle_shift")
}

#' @export
na_rle_shift.rle_na <- function(x, n = 1L) {
  rle_indices <- na_rle_indices(x)
  x[["indices"]] <- rle_indices + sign(n) * tunit(x) * abs(n)
  x
}

#' @export
na_rle_shift.list_of_rle_na <- function(x, n = 1L) {
  new_list_of_rle_na(!!! map(x, na_rle_shift.rle_na, n = n))
}

#' Expand and count run length encoding <`NA`>
#'
#' @inheritParams na_rle_shift
#'
#' @rdname mists-na-rle-tbl
#' @family rectangling functions
#' @examples
#' (x <- na_rle(c(1, NA, NA, 4:7, NA, NA, 10:15, NA)))
#' na_rle_expand(x)
#' na_rle_table(x)
#' @export
na_rle_expand <- function(x) {
  UseMethod("na_rle_expand")
}

#' @export
na_rle_expand.rle_na <- function(x) {
  res <- as_tibble(na_rle_reverse(x))
  indices_restore(res, x)
}

#' @export
na_rle_expand.list_of_rle_na <- function(x) {
  as_list_of(map(x, na_rle_expand))
}

#' @rdname mists-na-rle-tbl
#' @export
na_rle_table <- function(x) {
  UseMethod("na_rle_table")
}

#' @export
na_rle_table.rle_na <- function(x) {
  mutate(
    count(tibble("lengths" := na_rle_lengths(x)), lengths),
    "nobs" := n * lengths
  )
}

#' @export
na_rle_table.list_of_rle_na <- function(x) {
  as_list_of(map(x, na_rle_table))
}

#' Cut and aggregate run length encoding <`NA`>
#'
#' @inheritParams na_rle_expand
#' @param by A function applied to `indices`, such as tsibble's period functions
#' and lubridate's friends.
#'
#' @family rectangling functions
#' @return
#' A tibble contains:
#' * `indices`: aggregated indices.
#' * `n_run`: the number of unique runs for each `by`.
#' * `n_na`: the total number of `NA`s for each `by`.
#' @examples
#' if (!requireNamespace("nycflights13", quietly = TRUE)) {
#'   stop("Please install the nycflights13 package to run these following examples.")
#' }
#' if (!requireNamespace("tidyr", quietly = TRUE)) {
#'   stop("Please install the tidyr package to run these following examples.")
#' }
#' library(dplyr, warn.conflicts = FALSE)
#' nycflights13::weather %>% 
#'   group_by(origin) %>% 
#'   summarise(wind_gust_na = list_of_na_rle(wind_gust, time_hour)) %>% 
#'   mutate(wind_gust_na = na_rle_cut(wind_gust_na, by = tsibble::yearmonth)) %>% 
#'   tidyr::unnest(cols = wind_gust_na)
#' @export
na_rle_cut <- function(x, by) {
  UseMethod("na_rle_cut")
}

#' @export
na_rle_cut.rle_na <- function(x, by) {
  by <- as_function(by)
  tbl <- mutate(na_rle_expand(x), "indices2" := indices)
  grped_tbl <- group_by(tbl, "indices" := by(indices2))
  summarise(
    grped_tbl, 
    "n_run" := length(continuous_rle_impl(indices2, tunit(x))), 
    "n_na" := n()
  )
}

#' @export
na_rle_cut.list_of_rle_na <- function(x, by) {
  as_list_of(map(x, na_rle_cut, by))
}

continuous_rle_impl <- function(x, const) {
  x <- units_since(x)
  if (has_length(x, 0)) {
    0L
  } else if (has_length(x, 1)) {
    1L
  } else {
    diff_x <- diff(x) == const
    n <- length(x)
    tail_lgl <- tail(diff_x, 1)
    diff_x[n] <- if ((x[n] - x[n - 1]) == const) !tail_lgl else tail_lgl
    idx_switch <- which(!diff_x)
    c(idx_switch[1], diff(idx_switch))
  }
}

tbl_to_na_rle <- function(data, interval) {
  if (is_empty(data)) {
    return(na_rle(x = data[["indices"]], index_by = data[["lengths"]]))
  }

  rle_cont <- continuous_rle_impl(data[["indices"]], time_unit(interval))
  add_len <- mutate(data, "lengths" := rep.int(cumsum(rle_cont), rle_cont))
  red_data <- summarise(group_by(add_len, lengths), "indices" := min(indices))
  red_data <- indices_restore(red_data, data)
  new_rle_na(as.list(mutate(red_data, "lengths" := rle_cont)), interval)
}

# Work around for tsibble period functions not integrating vctrs yet
indices_restore <- function(x, to) {
  class(x$indices) <- class(to$indices)
  x
}

interval2 <- function(x) {
  x %@% "interval"
}

tunit <- function(x) {
  time_unit(interval2(x))
}

common_tunit <- function(x) {
  res <- vec_unique(map_dbl(x, tunit))
  if (vec_size(res) > 2) {
    abort("Elements in `list_of_na_rle()` don't have common intervals.")
  }
  max(res)
}

#' Set operations for run length encoding <`NA`>
#'
#' @param x,y Objects returned by [`na_rle()`] or [`list_of_na_rle()`].
#' @inheritParams dplyr::intersect
#'
#' @name set-op
#' @rdname mists-set-op
#' @examples
#' (x <- na_rle(c(1, NA, NA, 4:7, NA, NA, 10:15, NA)))
#' (y <- na_rle(c(10, NA, NA, NA, 6:3, NA, 1)))
#' intersect(x, y)
#' union(x, y)
#' setdiff(x, y)
#' setdiff(y, x)
#' labels <- c("x", "y", "intersect(x, y)", "union(x, y)", "setdiff(x, y)", 
#'   "setdiff(y, x)")
#' labels <- factor(labels, levels = labels)
#' lst_na_rle <- 
#'   vctrs::as_list_of(
#'     x, y,
#'     intersect(x, y),
#'     union(x, y),
#'     setdiff(x, y),
#'     setdiff(y, x)
#'   )
#' autoplot(lst_na_rle, labels)
#' @method intersect rle_na
#' @export
intersect.rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)[, "indices"]
  y_full <- na_rle_expand(y)[, "indices"]
  res <- intersect(x_full, y_full) # dplyr::intersect for data frame
  tbl_to_na_rle(indices_restore(res, x), interval2(x))
}

#' @method intersect list_of_rle_na
#' @export
intersect.list_of_rle_na <- function(x, y, ...) {
  new_list_of_rle_na(!!! map2(x, y, intersect.rle_na, ...))
}

#' @rdname mists-set-op
#' @method union rle_na
#' @export
union.rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)[, "indices"]
  y_full <- na_rle_expand(y)[, "indices"]
  res <- arrange(union(x_full, y_full), indices)
  tbl_to_na_rle(indices_restore(res, x), interval2(x))
}

#' @method union list_of_rle_na
#' @export
union.list_of_rle_na <- function(x, y, ...) {
  new_list_of_rle_na(!!! map2(x, y, union.rle_na, ...))
}

#' @rdname mists-set-op
#' @method setdiff rle_na
#' @export
setdiff.rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)[, "indices"]
  y_full <- na_rle_expand(y)[, "indices"]
  res <- setdiff(x_full, y_full)
  tbl_to_na_rle(indices_restore(res, x), interval2(x))
}

#' @method setdiff list_of_rle_na
#' @export
setdiff.list_of_rle_na <- function(x, y, ...) {
  new_list_of_rle_na(!!! map2(x, y, setdiff.rle_na, ...))
}

#' @export
length.rle_na <- function(x) { # for displaying the size of runs
  length(na_rle_lengths(x))
}

# `min()` and `max()` better to be defined through `vec_math()`, but seems that
# they require the input to be orderable. Not the case for rle_na, so
# overwrite `vctrs_vctr`.

#' @export
min.rle_na <- function(x, ...) {
  vec_math_base("min", na_rle_lengths(x))
}

#' @export
max.rle_na <- function(x, ...) {
  vec_math_base("max", na_rle_lengths(x))
}

#' @export
min.list_of_rle_na <- function(x, ...) {
  map_int(x, min)
}

#' @export
max.list_of_rle_na <- function(x, ...) {
  map_int(x, max)
}

#' @export
median.rle_na <- function(x, ...) {
  median(na_rle_lengths(x))
}

#' @export
median.list_of_rle_na <- function(x, ...) {
  map_dbl(x, median)
}

#' @export
quantile.rle_na <- function(x, ...) {
  quantile(na_rle_lengths(x), ...)
}

#' @export
quantile.list_of_rle_na <- function(x, ...) {
  map_dbl(x, quantile, ...)
}

#' @importFrom stats start
#' @export
start.rle_na <- function(x, ...) {
  na_rle_indices(x)
}

#' @export
start.list_of_rle_na <- function(x, ...) {
  res <- map(x, start)
  vec_c(!!! res)
}

#' @importFrom stats end
#' @export
end.rle_na <- function(x, ...) {
  vec_c(!!! na_rle_ends(x))
}

#' @export
end.list_of_rle_na <- function(x, ...) {
  vec_c(!!! map(x, end))
}

is_list_of_rle_na <- function(x) {
  inherits(x, "list_of_rle_na")
}

na_rle_reverse <- function(x) {
  if (is_empty(x)) {
    return(list("lengths" = x[["lengths"]], "indices" = x[["indices"]]))
  }
  rle_lengths <- na_rle_lengths(x)
  rle_indices <- na_rle_indices(x)
  full_seq <- map2(rle_indices, rle_lengths,
    function(.x, .y) seq(.x, by = tunit(x), length.out = .y))
  rep_lengths <- rep.int(rle_lengths, map_int(full_seq, vec_size))
  full_seq <- do.call("c", full_seq) # vec_c(!!! full_seq)
  list("lengths" = rep_lengths, "indices" = full_seq)
}
