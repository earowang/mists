globalVariables(c("n"))

#' Shift run length encoding <`NA`>
#'
#' @param x An object returned by [`na_rle()`] or [`list_of_na_rle()`].
#' @param n A positive integer shifts the starting indices forward, and negative
#' backward.
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
na_rle_shift.mists_rle_na <- function(x, n = 1L) {
  rle_values <- na_rle_values(x)
  tunit <- tunit(rle_values)
  x$values <- rle_values + sign(n) * tunit
  x
}

#' @export
na_rle_shift.mists_list_of_rle_na <- function(x, n = 1L) {
  map(x, na_rle_shift.mists_rle_na, n = n)
}

#' Expand and count run length encoding <`NA`>
#'
#' @inheritParams na_rle_shift
#' @param ... Passed to individual methods.
#'
#' @rdname mists-na-rle-tbl
#' @examples
#' (x <- na_rle(c(1, NA, NA, 4:7, NA, NA, 10:15, NA)))
#' (y <- na_rle(c(10, NA, NA, NA, 6:3, NA, 1)))
#' na_rle_expand(x)
#' na_rle_expand(y)
#' na_rle_expand(vctrs::as_list_of(x, y), group = c("x", "y"))
#' na_rle_table(x)
#' @export
na_rle_expand <- function(x, ...) {
  UseMethod("na_rle_expand")
}

#' @export
na_rle_expand.mists_rle_na <- function(x, ...) {
  if (is_empty(x)) {
    return(tibble("lengths" = integer(0), "values" = integer(0)))
  }
  rle_lengths <- na_rle_lengths(x)
  rle_values <- na_rle_values(x)
  tunit <- tunit(rle_values)
  full_seq <- map2(rle_values, rle_lengths,
    function(.x, .y) seq(.x, by = tunit, length.out = .y))
  rep_lengths <- rep.int(rle_lengths, map_int(full_seq, vec_size))
  full_seq <- do.call("c", full_seq)
  res <- tibble("lengths" = rep_lengths, "values" = full_seq)
  interval_restore(res, x)
}

#' @export
na_rle_expand.mists_list_of_rle_na <- function(x, ...) {
  qs <- enquos(..., .named = TRUE)
  y <- eval_tidy(qs[[1]])
  stopifnot(vec_size(x) == vec_size(y))
  res <- bind_rows(
    map2(x, y, function(.x, .y) mutate(na_rle_expand(.x), !! names(qs) := .y))
  )
  interval_restore(res, x[[1L]])
}

#' @rdname mists-na-rle-tbl
#' @export
na_rle_table <- function(x) {
  mutate(
    count(tibble(lengths = na_rle_lengths(x)), lengths),
    nobs = n * lengths
  )
}

tbl_to_na_rle <- function(data) {
  vals <- data[["values"]]
  if (is_empty(vals)) return(na_rle()) # should also find "lengths" type

  rle_cont <- continuous_rle_impl(vals, tunit(vals))
  add_len <- mutate(data, lengths = rep.int(cumsum(rle_cont), rle_cont))
  red_data <- summarise(group_by(add_len, lengths), values = min(values))
  red_data <- interval_restore(red_data, data)
  new_mists_rle_na(as_list(mutate(red_data, lengths = rle_cont)))
}

interval_restore <- function(x, to) {
  attr(x$values, "interval") <- attr(to$values, "interval")
  x
}

tunit <- function(values) {
  time_unit(values %@% "interval")
}

#' Set operations for run length encoding <`NA`>
#'
#' @param x,y Objects returned by [`na_rle()`].
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
#' @method intersect mists_rle_na
#' @export
intersect.mists_rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)[, "values"]
  y_full <- na_rle_expand(y)[, "values"]
  res <- intersect(x_full, y_full) # dplyr::intersect for data frame
  tbl_to_na_rle(interval_restore(res, x))
}

#' @rdname mists-set-op
#' @method union mists_rle_na
#' @export
union.mists_rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)[, "values"]
  y_full <- na_rle_expand(y)[, "values"]
  res <- arrange(union(x_full, y_full), values)
  tbl_to_na_rle(interval_restore(res, x))
}

#' @rdname mists-set-op
#' @method setdiff mists_rle_na
#' @export
setdiff.mists_rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)[, "values"]
  y_full <- na_rle_expand(y)[, "values"]
  res <- setdiff(x_full, y_full)
  tbl_to_na_rle(interval_restore(res, x))
}
