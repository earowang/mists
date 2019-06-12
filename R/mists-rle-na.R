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
#' @return 
#' An object of class `rle_na` or `list_of_rle_na`. A named list of:
#' * `lengths`: the lengths of `NA` runs
#' * `indices`: the starting indices of runs
#'
#' @section Mathematical operations:
#' Many math operations can be applied to objects returned from `na_rle()` and
#' `list_of_na_rle()`, regarding the *lengths* of runs.
#' * `sum()`: the total number of `NA` over all runs.
#' * `mean()`: the average `NA`s per run.
#' * `min()` & `max()`: the minimum and maximum of runs.
#' * `median()` & `quantile()`
#' * Other [Math] group generic
#'
#' @rdname na-rle
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' df <- tibble(year = 2000:2019, temp = sample(0:30, size = 20))
#' df[c(1, 6, 13:16, 19), "temp"] <- NA
#' df
#'
#' na_rle(df$temp) # indexed by the default positions
#' (x <- na_rle(df$temp, index_by = df$year)) # indexed by a variable
#'
#' na_rle_lengths(x)
#' na_rle_indices(x)
#' na_rle_ends(x)
#'
#' length(x) # the number of runs
#' sum(x) # the total number of `NA`
#' range(x) # min & max runs
#'
#' # list_of_na_rle() is useful when working with tabular data
#' na_rle_df <- df %>% 
#'   mutate(group = rep(letters[1:2], each = 10)) %>% 
#'   group_by(group) %>% 
#'   summarise(na_runs = list_of_na_rle(temp, year))
#' na_rle_df
#'
#' sum(na_rle_df$na_runs)
#' range(na_rle_df$na_runs)
#' @export
na_rle <- function(x = double(), index_by = seq_along(x), interval = NULL) {
  stopifnot(vec_size(x) == vec_size(index_by))
  if (vec_is_empty(x)) {
    indices <- index_by
    attr(indices, "interval") <- interval_pull(index_by)
    return(new_rle_na(list(lengths = integer(), indices = indices)))
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
  from <- c(1L, head(cumsum(res[["lengths"]]), -1L) + 1L)[res[["values"]]]
  indices <- index_by[ord][from]
  attr(indices, "interval") <- int
  new_rle_na(list(
    lengths = res[["lengths"]][res[["values"]]],
    indices = indices
  ))
}

#' @rdname na-rle
#' @export
list_of_na_rle <- function(x = double(), index_by = seq_along(x),
  interval = NULL) {
  new_list_of_rle_na(
    !!! list2(na_rle(x, index_by = index_by, interval = interval))
  )
}

#' @rdname na-rle
#' @export
na_rle_lengths <- function(x) {
  UseMethod("na_rle_lengths")
}

#' @export
na_rle_lengths.rle_na <- function(x) {
  x[["lengths"]]
}

#' @export
na_rle_lengths.list_of_rle_na <- function(x) {
  as_list_of(map(x, na_rle_lengths))
}

#' @rdname na-rle
#' @export
na_rle_indices <- function(x) {
  UseMethod("na_rle_indices")
}

#' @export
na_rle_indices.rle_na <- function(x) {
  x[["indices"]]
}

#' @export
na_rle_indices.list_of_rle_na <- function(x) {
  as_list_of(map(x, na_rle_indices.rle_na))
}

#' @rdname na-rle
#' @export
na_rle_ends <- function(x) {
  UseMethod("na_rle_ends")
}

#' @export
na_rle_ends.rle_na <- function(x) {
  rle_lengths <- na_rle_lengths(x)
  rle_indices <- na_rle_indices(x)
  tunit <- tunit(rle_indices)
  vec_c(!!! map2(rle_indices, rle_lengths, function(.x, .y) .x + tunit * .y))
}

#' @export
na_rle_ends.list_of_rle_na <- function(x) {
  as_list_of(map(x, na_rle_ends.rle_na))
}

new_rle_na <- function(x) {
  rle_na_assert(x)
  new_vctr(x, class = "rle_na")
}

new_list_of_rle_na <- function(...) {
  new_list_of(
    list2(...),
    ptype = list(),
    class = "list_of_rle_na"
  )
}

rle_na_assert <- function(x) {
  if (is_false(is_bare_list(x) && all(has_name(x, c("lengths", "indices"))))) {
    abort("Run length encoding must be a named list with `lengths` and `indices`.")
  }
  if (is_null(x[["indices"]] %@% "interval")) {
    abort("Missing \"interval\".")
  }
}
