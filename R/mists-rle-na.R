na_rle_impl <- function(x) {
  rle(is.na(x))
}

#' Run lengths encoding for missing values (`NA`)
#'
#' @param x A vector.
#' @param index_by A vector of the same length as `x`.
#' @param interval if `NULL`, determined by the greatest common denominator;
#' otherwise a supplied "interval" class. See `?tsibble::tsibble` for details.
#' @inheritParams vctrs::as_list_of
#' 
#' @return A named list of
#' * `lengths`: the lengths of `NA` runs
#' * `indices`: the starting indices of runs
#'
#' @section Mathematical operations:
#' Many math operations can be applied to objects returned from `na_rle()` and 
#' `list_of_na_rle()`, regarding the *lengths* of runs.
#' * `sum()`: the total number of `NA` over all runs.
#' * `mean()`: the average `NA`s per run.
#' * `min()` & `max()`: the minimum and maximum of runs.
#'
#' @rdname na-rle
#' @examples
#' df <- data.frame(year = 2000:2019, temp = sample(0:30, size = 10))
#' df[c(1, 6, 13:16, 19), "temp"] <- NA
#' 
#' na_rle(df$temp) # indexed by the default positions
#' (x <- na_rle(df$temp, index_by = df$year)) # indexed by a variable
#' 
#' na_rle_lengths(x)
#' na_rle_indices(x)
#'
#' length(x) # the number of runs
#' sum(x) # the total number of `NA`
#' range(x) # min & max runs
#' 
#' library(dplyr, warn.conflicts = FALSE)
#' # list_of_na_rle() is useful when working with tabular data
#' na_rle_df <- as_tibble(df) %>% 
#'   summarise(na_runs = list_of_na_rle(temp, year))
#' na_rle_df
#'
#' length(na_rle_df$na_runs)
#' sum(na_rle_df$na_runs)
#' range(na_rle_df$na_runs)
#' @export
na_rle <- function(x = double(), index_by = seq_along(x), interval = NULL) {
  stopifnot(vec_size(x) == vec_size(index_by))
  if (vec_is_empty(x)) {
    indices <- index_by
    attr(indices, "interval") <- interval_pull(index_by)
    return(new_mists_rle_na(list(lengths = integer(), indices = indices)))
  }

  if (vec_duplicate_any(index_by)) {
    abort("`index_by` only takes unique values.")
  }
  ord <- vec_order(index_by)
  x <- x[ord]
  if (is_null(interval)) {
    int <- interval_pull(index_by) 
  } else {
    if (!inherits(interval, "interval")) {
      abort("`interval` must be class interval.")
    }
    int <- interval
  }

  res <- na_rle_impl(x)
  from <- c(1L, head(cumsum(res$lengths), -1L) + 1L)[res$values]
  indices <- index_by[ord][from]
  attr(indices, "interval") <- int
  new_mists_rle_na(list(lengths = res$lengths[res$values], indices = indices))
}

#' @rdname na-rle
#' @export
na_rle_lengths <- function(x) {
  UseMethod("na_rle_lengths")
}

#' @export
na_rle_lengths.mists_rle_na <- function(x) {
  x$lengths
}

#' @export
na_rle_lengths.mists_list_of_rle_na <- function(x) {
  as_list_of(map(x, na_rle_lengths))
}

#' @rdname na-rle
#' @export
na_rle_indices <- function(x) {
  UseMethod("na_rle_indices")
}

#' @export
na_rle_indices.mists_rle_na <- function(x) {
  x$indices
}

#' @export
na_rle_indices.mists_list_of_rle_na <- function(x) {
  as_list_of(map(x, na_rle_indices))
}

#' @rdname na-rle
#' @export
list_of_na_rle <- function(x = double(), index_by = seq_along(x),
  interval = NULL) {
  new_list_of(
    list(na_rle(x, index_by = index_by, interval = interval)),
    ptype = list(),
    class = "mists_list_of_rle_na"
  )
}

#' @rdname na-rle
#' @method as_list_of mists_rle_na
#' @export
#' @export as_list_of.mists_rle_na
as_list_of.mists_rle_na <- function(x, ...) {
  new_list_of(
    list(x, ...),
    ptype = list(),
    class = "mists_list_of_rle_na"
  )
}

new_mists_rle_na <- function(x) {
  mists_rle_na_assert(x)
  new_vctr(x, class = c("mists_rle_na"))
}

mists_rle_na_assert <- function(x) {
  if (is_false(is_bare_list(x) && all(has_name(x, c("lengths", "indices"))))) {
    abort("Run length encoding must be a named list with `lengths` and `indices`.")
  }
}

#' @export
length.mists_rle_na <- function(x) { # for displaying the size of runs
  length(na_rle_lengths(x))
}

# `min()` and `max()` better to be defined through `vec_math()`, but seems that
# they require the input to be orderable. Not the case for mists_rle_na, so
# overwrite `vctrs_vctr`.

#' @export
min.mists_rle_na <- function(x, ...) {
  vec_math_base("min", na_rle_lengths(x))
}

#' @export
max.mists_rle_na <- function(x, ...) {
  vec_math_base("max", na_rle_lengths(x))
}

#' @export
min.mists_list_of_rle_na <- function(x, ...) {
  map_int(x, min)
}

#' @export
max.mists_list_of_rle_na <- function(x, ...) {
  map_int(x, max)
}
