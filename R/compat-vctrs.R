#' vctrs compatibility functions
#'
#' These functions are the extensions that allow mists objects to
#' work with vctrs.
#'
#' @param x Objects.
#' @param to Type to cast to.
#' @param ... Used to pass along error message information.
#' @inheritParams vec_type2
#'
#' @name vctrs-compat
#'
NULL

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_cast.list mists_rle_na
#' @export
#' @export vec_cast.list.mists_rle_na
vec_cast.list.mists_rle_na <- function(x, to) {
  as_list_of(x)
}

#' @rdname vctrs-compat
#' @keywords internal
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

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_ptype_full mists_rle_na
#' @export
#' @export vec_ptype_full.mists_rle_na
vec_ptype_full.mists_rle_na <- function(x) {
  "Run Length Encoding <NA>"
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_ptype_abbr mists_rle_na
#' @export
#' @export vec_ptype_abbr.mists_rle_na
vec_ptype_abbr.mists_rle_na <- function(x) {
  "rle<NA>"
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_ptype_abbr mists_list_of_rle_na
#' @export
#' @export vec_ptype_abbr.mists_list_of_rle_na
vec_ptype_abbr.mists_list_of_rle_na <- function(x) {
  "list<rle<NA>>"
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_ptype_full mists_list_of_rle_na
#' @export
#' @export vec_ptype_full.mists_list_of_rle_na
vec_ptype_full.mists_list_of_rle_na <- function(x) {
  "list_of<Run Length Encoding <NA>>"
}
