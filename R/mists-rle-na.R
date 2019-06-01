#' @export
na_rle <- function(x = double(), index_by = NULL, interval = NULL) {
  if (has_length(x, 0)) {
    # ToDo: if !is_null(index_by) values takes an empty index_by obj
    return(new_mists_rle_na(list(lengths = integer(), values = integer())))
  }

  if (is_null(index_by)) {
    index_by <- seq_along(x)
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

#' @export
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

#' @export
vec_ptype_full.mists_rle_na <- function(x) {
  "Run Length Encoding <NA>"
}

#' @export
vec_ptype_abbr.mists_rle_na <- function(x) {
  "rle<NA>"
}

#' @export
list_of_na_rle <- function(x, index_by = NULL, interval = NULL) {
  new_list_of(
    list(na_rle(x, index_by = index_by, interval = interval)),
    ptype = list(),
    class = "mists_list_of_rle_na"
  )
}

#' @export
as_list_of.mists_rle_na <- function(x, ...) {
  new_list_of(
    list(x, ...),
    ptype = list(),
    class = "mists_list_of_rle_na"
  )
}

#' @export
vec_ptype_abbr.mists_list_of_rle_na <- function(x) {
  "list<rle<NA>>"
}

#' @export
vec_ptype_full.mists_list_of_rle_na <- function(x) {
  "list_of<Run Length Encoding <NA>>"
}
