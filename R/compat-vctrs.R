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
  new_list_of_rle_na(x, ...)
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_type2 rle_na
#' @export
#' @export vec_type2.rle_na
vec_type2.rle_na <- function(x, y, ...) {
  UseMethod("vec_type2.rle_na", y)
}

#' @method vec_type2.rle_na default
#' @export
vec_type2.rle_na.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_incompatible_type(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_type2.rle_na rle_na
#' @export
vec_type2.rle_na.rle_na <- function(x, y, ...) {
  stop_incompatible_type(
    x, y, ...,
    details = "Please use `vctrs::as_list_of()` instead."
  )
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_type2 list_of_rle_na
#' @export
#' @export vec_type2.list_of_rle_na
vec_type2.list_of_rle_na <- function(x, y, ...) {
  UseMethod("vec_type2.list_of_rle_na", y)
}

#' @method vec_type2.list_of_rle_na default
#' @export
vec_type2.list_of_rle_na.default <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_incompatible_type(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}

# #' @method vec_type2.list_of_rle_na rle_na
# #' @export
# vec_type2.list_of_rle_na.rle_na <- function(x, y, ...) {
#   new_list_of_rle_na(list2(!!! x, y))
# }

# #' @method vec_type2.list_of_rle_na list_of_rle_na
# #' @export
# vec_type2.list_of_rle_na.list_of_rle_na <- function(x, y, ...) {
#   new_list_of_rle_na()
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
#' @method vec_restore rle_na
#' @export
#' @export vec_restore.rle_na
vec_restore.rle_na <- function(x, to, ..., n = NULL) {
  new_rle_na(vec_data(x), interval = to %@% "interval")
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method obj_print_data rle_na
#' @export
#' @export obj_print_data.rle_na
obj_print_data.rle_na <- function(x, ...) {
  over10 <- vec_size(x) > 10
  len <- na_rle_lengths(x)
  idx <- na_rle_indices(x)
  cat(
    "$lengths:", angle_brackets(vec_ptype_abbr(len)), 
    if (over10) ellipsis(len[1:10]) else len, "\n"
  )
  cat(
    "$indices:", angle_brackets(vec_ptype_abbr(idx)),
    if (over10) ellipsis(format(idx[1:10])) else format(idx)
  )
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
