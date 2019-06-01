#' @export
na_rle_expand <- function(x) {
  rle_lengths <- na_rle_lengths(x)
  rle_values <- na_rle_values(x)
  tunit <- time_unit(rle_values %@% "interval")
  full_seq <- map2(rle_values, rle_lengths,
    function(.x, .y) seq(.x, by = tunit, length.out = .y))
  rep_lengths <- rep.int(rle_lengths, map_int(full_seq, vec_size))
  full_seq <- do.call("c", full_seq)
  dplyr::tibble("lengths" = rep_lengths, "values" = full_seq)
}

#' @export
na_rle_table <- function(x) {
  mutate(
    dplyr::count(dplyr::tibble(lengths = na_rle_lengths(x)), lengths),
    nobs = n * lengths
  )
}

#' @importFrom dplyr intersect
#' @export
intersect.mist_rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)
  y_full <- na_rle_expand(y)
  semi_join(x_full, y_full, by = "values")
}

#' @importFrom dplyr union
#' @export
union.mist_rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)
  y_full <- na_rle_expand(y)
  full_join(x_full, y_full, by = names(x_full))
}

#' @importFrom dplyr union_all
#' @export
union_all.mist_rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)
  y_full <- na_rle_expand(y)
  dplyr::bind_rows(x_full, y_full)
}

#' @importFrom dplyr setdiff
#' @export
setdiff.mist_rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)
  y_full <- na_rle_expand(y)
  anti_join(x_full, y_full, by = "values")
}
