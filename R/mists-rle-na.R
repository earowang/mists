#' Run lengths encoding for missing values (`NA`)
#'
#' @param x A vector.
#' @param index_by A vector of the same length as `x`.
#' @param interval if `NULL`, determined by the greatest common denominator;
#' otherwise a supplied "interval" class.
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
    # ToDo: if !is_null(index_by) values takes an empty index_by obj
    return(new_mists_rle_na(list(lengths = integer(), values = integer())))
  }

  ord <- order(index_by)
  x <- x[ord]
  # ToDo:
  # 1. abort unknown/irregular interval
  # 2. abort if not an interval class when `interval` is supplied
  if (is_null(interval)) int <- interval_pull(index_by) else int <- interval

  res <- na_rle_cpp(x)
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
list_of_na_rle <- function(x, index_by = NULL, interval = NULL) {
  new_list_of(
    list(na_rle(x, index_by = index_by, interval = interval)),
    ptype = list(),
    class = "mists_list_of_rle_na"
  )
}

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

#' @method obj_print_data mists_rle_na
#' @export
#' @export obj_print_data.mists_rle_na
obj_print_data.mists_rle_na <- function(x, ...) {
  over10 <- length(x) > 10
  cat(
    "$lengths:", angle_brackets(vec_ptype_abbr(x$lengths)), 
    if (over10) ellipsis(x$lengths[1:10]) else x$lengths, "\n"
  )
  cat(
    "$values :", angle_brackets(vec_ptype_abbr(x$values)),
    if (over10) ellipsis(format(x$values[1:10])) else format(x$values)
  )
  invisible(x)
}

#' @method vec_ptype_full mists_rle_na
#' @export
#' @export vec_ptype_full.mists_rle_na
vec_ptype_full.mists_rle_na <- function(x) {
  "Run Length Encoding <NA>"
}

#' @method vec_ptype_abbr mists_rle_na
#' @export
#' @export vec_ptype_abbr.mists_rle_na
vec_ptype_abbr.mists_rle_na <- function(x) {
  "rle<NA>"
}

#' @method vec_ptype_abbr mists_list_of_rle_na
#' @export
#' @export vec_ptype_abbr.mists_list_of_rle_na
vec_ptype_abbr.mists_list_of_rle_na <- function(x) {
  "list<rle<NA>>"
}

#' @method vec_ptype_full mists_list_of_rle_na
#' @export
#' @export vec_ptype_full.mists_list_of_rle_na
vec_ptype_full.mists_list_of_rle_na <- function(x) {
  "list_of<Run Length Encoding <NA>>"
}
