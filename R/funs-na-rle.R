globalVariables(c("n"))

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
  tunit <- tunit(rle_indices)
  x[["indices"]] <- rle_indices + sign(n) * tunit * abs(n)
  x
}

#' @export
na_rle_shift.list_of_rle_na <- function(x, n = 1L) {
  new_list_of_rle_na(!!! map(x, na_rle_shift.rle_na, n = n))
}

#' Expand and count run length encoding <`NA`>
#'
#' @inheritParams na_rle_shift
#' @param ... Passed to individual methods.
#'
#' @rdname mists-na-rle-tbl
#' @examples
#' (x <- na_rle(c(1, NA, NA, 4:7, NA, NA, 10:15, NA)))
#' na_rle_expand(x)
#' na_rle_table(x)
#' @export
na_rle_expand <- function(x, ...) {
  UseMethod("na_rle_expand")
}

#' @export
na_rle_expand.rle_na <- function(x, ...) {
  if (is_empty(x)) {
    return(tibble("lengths" := x[["lengths"]], "indices" := x[["indices"]]))
  }
  rle_lengths <- na_rle_lengths(x)
  rle_indices <- na_rle_indices(x)
  tunit <- tunit(rle_indices)
  full_seq <- map2(rle_indices, rle_lengths,
    function(.x, .y) seq(.x, by = tunit, length.out = .y))
  rep_lengths <- rep.int(rle_lengths, map_int(full_seq, vec_size))
  full_seq <- do.call("c", full_seq) # vec_c(!!! full_seq)
  res <- tibble("lengths" := rep_lengths, "indices" := full_seq)
  indices_restore(res, x)
}

#' @export
na_rle_expand.list_of_rle_na <- function(x, ...) {
  qs <- enquos(..., .named = TRUE)
  if (is_empty(qs)) {
    y <- vec_seq_along(x)
    new_col <- "id"
  } else {
    y <- eval_tidy(qs[[1]])
    new_col <- names(qs)
  }
  res_lst <-
    map2(x, y, function(.x, .y) mutate(na_rle_expand(.x), !! new_col := .y))
  res <- bind_rows(!!! res_lst) # vec_rbind() should work here
  indices_restore(res, x[[1L]])
}

#' @rdname mists-na-rle-tbl
#' @export
na_rle_table <- function(x) {
  mutate(
    count(tibble("lengths" := na_rle_lengths(x)), lengths),
    "nobs" := n * lengths
  )
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

tbl_to_na_rle <- function(data) {
  if (is_empty(data)) {
    return(na_rle(x = data[["indices"]], index_by = data[["lengths"]]))
  }

  rle_cont <- continuous_rle_impl(data[["indices"]], tunit(data[["indices"]]))
  add_len <- mutate(data, "lengths" := rep.int(cumsum(rle_cont), rle_cont))
  red_data <- summarise(group_by(add_len, lengths), "indices" := min(indices))
  red_data <- indices_restore(red_data, data)
  new_rle_na(as_list(mutate(red_data, "lengths" := rle_cont)))
}

indices_restore <- function(x, to) {
  class(x$indices) <- class(to$indices)
  attr(x$indices, "interval") <- attr(to$indices, "interval")
  x
}

tunit <- function(indices) {
  time_unit(indices %@% "interval")
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
#' @method intersect rle_na
#' @export
intersect.rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)[, "indices"]
  y_full <- na_rle_expand(y)[, "indices"]
  res <- intersect(x_full, y_full) # dplyr::intersect for data frame
  tbl_to_na_rle(indices_restore(res, x))
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
  tbl_to_na_rle(indices_restore(res, x))
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
  tbl_to_na_rle(indices_restore(res, x))
}

#' @export
as_tibble.rle_na <- function(x, ...) {
  as_tibble(unclass(x))
}

#' @export
as_tibble.list_of_rle_na <- function(x, ...) {
  y <- seq_along(x)
  res <- map2(x, y, function(.x, .y) mutate(as_tibble.rle_na(.x), "id" := .y))
  vec_rbind(!!! res)
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
