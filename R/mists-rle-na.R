na_rle_impl <- function(x) {
  rle(is.na(x))
}

#' Run lengths encoding for missing values (`NA`)
#'
#' @param x A vector.
#' @param index_by A vector of the same length as `x`.
#' @param interval if `NULL`, determined by the greatest common denominator;
#' otherwise a supplied "interval" class. See `?tsibble::tsibble` for details.
#' 
#' @return A named list of
#' * `lengths`: the lengths of `NA` runs
#' * `values`: the starting indices of runs
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
#' na_rle_values(x)
#' 
#' library(dplyr, warn.conflicts = FALSE)
#' # list_of_na_rle() is useful when working with tabular data
#' as_tibble(df) %>% 
#'   summarise(na_runs = list_of_na_rle(temp, year))
#' @export
na_rle <- function(x = double(), index_by = seq_along(x), interval = NULL) {
  stopifnot(vec_size(x) == vec_size(index_by))
  if (has_length(x, 0)) {
    values <- index_by
    attr(values, "interval") <- interval_pull(index_by)
    return(new_mists_rle_na(list(lengths = integer(), values = values)))
  }

  if (vec_duplicate_any(index_by)) {
    abort("`index_by` only takes unique values.")
  }
  ord <- order(index_by)
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
  values <- index_by[ord][from]
  attr(values, "interval") <- int
  new_mists_rle_na(list(lengths = res$lengths[res$values], values = values))
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
  map(x, na_rle_lengths)
}

#' @rdname na-rle
#' @export
na_rle_values <- function(x) {
  UseMethod("na_rle_values")
}

#' @export
na_rle_values.mists_rle_na <- function(x) {
  x$values
}

#' @export
na_rle_values.mists_list_of_rle_na <- function(x) {
  map(x, na_rle_values)
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
  if (is_false(is_bare_list(x) && has_name(x, c("lengths", "values")))) {
    abort("Run length encoding must be a named list with `lengths` and `values`.")
  }
}

#' @export
length.mists_rle_na <- function(x) { # for displaying the size of runs
  length(x$lengths)
}
