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
na_rle_shift.mists_rle_na <- function(x, n = 1L) {
  rle_indices <- na_rle_indices(x)
  tunit <- tunit(rle_indices)
  x$indices <- rle_indices + sign(n) * tunit * abs(n)
  x
}

#' @export
na_rle_shift.mists_list_of_rle_na <- function(x, n = 1L) {
  as_list_of(map(x, na_rle_shift.mists_rle_na, n = n))
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
    return(tibble("lengths" = integer(0), "indices" = integer(0)))
  }
  rle_lengths <- na_rle_lengths(x)
  rle_indices <- na_rle_indices(x)
  tunit <- tunit(rle_indices)
  full_seq <- map2(rle_indices, rle_lengths,
    function(.x, .y) seq(.x, by = tunit, length.out = .y))
  rep_lengths <- rep.int(rle_lengths, map_int(full_seq, vec_size))
  full_seq <- do.call("c", full_seq)
  res <- tibble("lengths" = rep_lengths, "indices" = full_seq)
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

continuous_rle_impl <- function(x, const) {
  x <- as.double(x)
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
  add_len <- mutate(data, lengths = rep.int(cumsum(rle_cont), rle_cont))
  red_data <- summarise(group_by(add_len, lengths), indices = min(indices))
  red_data <- interval_restore(red_data, data)
  new_mists_rle_na(as_list(mutate(red_data, lengths = rle_cont)))
}

interval_restore <- function(x, to) {
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
#' @method intersect mists_rle_na
#' @export
intersect.mists_rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)[, "indices"]
  y_full <- na_rle_expand(y)[, "indices"]
  res <- intersect(x_full, y_full) # dplyr::intersect for data frame
  tbl_to_na_rle(interval_restore(res, x))
}

#' @method intersect mists_list_of_rle_na
#' @export
intersect.mists_list_of_rle_na <- function(x, y, ...) {
  as_list_of(map2(x, y, intersect.mists_rle_na, ...))
}

#' @rdname mists-set-op
#' @method union mists_rle_na
#' @export
union.mists_rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)[, "indices"]
  y_full <- na_rle_expand(y)[, "indices"]
  res <- arrange(union(x_full, y_full), indices)
  tbl_to_na_rle(interval_restore(res, x))
}

#' @method union mists_list_of_rle_na
#' @export
union.mists_list_of_rle_na <- function(x, y, ...) {
  as_list_of(map2(x, y, union.mists_rle_na, ...))
}

#' @rdname mists-set-op
#' @method setdiff mists_rle_na
#' @export
setdiff.mists_rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)[, "indices"]
  y_full <- na_rle_expand(y)[, "indices"]
  res <- setdiff(x_full, y_full)
  tbl_to_na_rle(interval_restore(res, x))
}

#' @method setdiff mists_list_of_rle_na
#' @export
setdiff.mists_list_of_rle_na <- function(x, y, ...) {
  as_list_of(map2(x, y, setdiff.mists_rle_na, ...))
}
