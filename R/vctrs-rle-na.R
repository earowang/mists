na_rle <- function(x = double(), order_by = NULL) {
  if (has_length(x, 0)) {
    return(new_rle_na(list(lengths = integer(), values = integer())))
  }

  if (!is_null(order_by)) {
    ord <- order(order_by)
    x <- x[ord]
  }
  res <- na_rle_cpp(x)
  from <- c(1L, head(cumsum(res$lengths), -1L) + 1L)[res$values]
  new_rle_na(list(
    lengths = res$lengths[res$values], 
    values = if (is_null(order_by)) from else order_by[ord][from]
  ))
}

obj_print_data.vctrs_rle_na <- function(x, ...) {
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

new_rle_na <- function(x) {
  vctrs_rle_na_assert(x)
  new_vctr(x, class = c("vctrs_rle_na"))
}

vctrs_rle_na_assert <- function(x) {
  if (is_false(is_bare_list(x) && has_name(x, c("lengths", "values")))) {
    abort("Run length encoding must be a named list with `lengths` and `values`.")
  }
}

length.vctrs_rle_na <- function(x) {
  length(x$lengths)
}

vec_ptype_full.vctrs_rle_na <- function(x) {
  "Run Length Encoding <NA>"
}

vec_ptype_abbr.vctrs_rle_na <- function(x) {
  "rle<NA>"
}

list_of_na_rle <- function(x, order_by = NULL) {
  new_list_of(
    list(na_rle(x, order_by = order_by)),
    ptype = list(),
    class = "vctrs_list_of_rle_na"
  )
}

vec_ptype_abbr.vctrs_list_of_rle_na <- function(x) {
  "list<rle<NA>>"
}

vec_ptype_full.vctrs_list_of_rle_na <- function(x) {
  "list_of<rle<NA>>"
}
