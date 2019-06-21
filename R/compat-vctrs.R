#' vctrs compatibility functions
#'
#' These functions are the extensions that allow mists objects to
#' work with vctrs.
#'
#' @param x Objects.
#' @param to Type to cast to.
#' @param ... Used to pass along error message information.
#'
#' @name vctrs-compat
#'
NULL

#' @rdname vctrs-compat
#' @rdname vctrs-compat
#' @method as_list_of rle_na
#' @export
#' @export as_list_of.rle_na
as_list_of.rle_na <- function(x, ...) {
  new_list_of_rle_na(!!! list(x, ...))
}

# #' @rdname vctrs-compat
# #' @keywords internal
# #' @method vec_type2 rle_na
# #' @export
# #' @export vec_type2.rle_na
# vec_type2.rle_na <- function(x, y, ...) {
#   UseMethod("vec_type2.rle_na", y)
# }
#
# #' @method vec_type2.rle_na rle_na
# #' @export
# vec_type2.rle_na.rle_na <- function(x, y, ...) {
#   new_list_of_rle_na(x, y)
# }

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_cast.list rle_na
#' @export
#' @export vec_cast.list.rle_na
vec_cast.list.rle_na <- function(x, to, ...) {
  as_list_of(x)
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_math rle_na
#' @export
#' @export vec_math.rle_na
vec_math.rle_na <- function(.fn, x, ...) {
  na_rle_lengths_x <- na_rle_lengths(x)
  vec_math_base(.fn, na_rle_lengths_x, ...)
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_math list_of_rle_na
#' @export
#' @export vec_math.list_of_rle_na
vec_math.list_of_rle_na <- function(.fn, x, ...) {
  na_rle_lengths_x <- na_rle_lengths(x)
  # bug in vctrs::vec_math? `x` becomes a list of lists
  if (vec_depth(na_rle_lengths_x) == 3) { # sum()
    na_rle_lengths_x <- vec_c(!!! na_rle_lengths_x)
  }
  map_dbl(na_rle_lengths_x, function(.x) vec_math_base(.fn, .x, ...))
}

# #' @rdname vctrs-compat
# #' @keywords internal
# #' @method vec_proxy rle_na
# #' @export
# #' @export vec_proxy.rle_na
# vec_proxy.rle_na <- function(x) {
#   new_data_frame(unclass(x))
# }

# #' @rdname vctrs-compat
# #' @keywords internal
# #' @method vec_restore rle_na
# #' @export
# #' @export vec_restore.rle_na
# vec_restore.rle_na <- function(x, to, ..., i = NULL) {
#   new_rle_na(x)
# }

#' @rdname vctrs-compat
#' @keywords internal
#' @method obj_print_data rle_na
#' @export
#' @export obj_print_data.rle_na
obj_print_data.rle_na <- function(x, ...) {
  over10 <- length(x) > 10
  cat(
    "$lengths:", angle_brackets(vec_ptype_abbr(x[["lengths"]])), 
    if (over10) ellipsis(x[["lengths"]][1:10]) else x[["lengths"]], "\n"
  )
  cat(
    "$indices:", angle_brackets(vec_ptype_abbr(x[["indices"]])),
    if (over10) ellipsis(format(x[["indices"]][1:10])) else format(x[["indices"]])
  )
  invisible(x)
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method obj_print_header rle_na
#' @export
#' @export obj_print_header.rle_na
obj_print_header.rle_na <- function(x, ...) {
  cat_line("<", vec_ptype_full(x), "[", length(x), "]>")
  invisible(x)
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_ptype_full rle_na
#' @export
#' @export vec_ptype_full.rle_na
vec_ptype_full.rle_na <- function(x) {
  "Run Length Encoding <NA>"
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_ptype_abbr rle_na
#' @export
#' @export vec_ptype_abbr.rle_na
vec_ptype_abbr.rle_na <- function(x) {
  "rle<NA>"
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_ptype_abbr list_of_rle_na
#' @export
#' @export vec_ptype_abbr.list_of_rle_na
vec_ptype_abbr.list_of_rle_na <- function(x) {
  "list<rle<NA>>"
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_ptype_full list_of_rle_na
#' @export
#' @export vec_ptype_full.list_of_rle_na
vec_ptype_full.list_of_rle_na <- function(x) {
  "list_of<Run Length Encoding <NA>>"
}
