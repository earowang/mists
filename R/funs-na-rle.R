#' @export
na_rle_shift <- function(x, n = 1L) {
  if (n == 0) return(x)
  UseMethod("na_rle_shift")
}

#' @export
na_rle_shift.mists_rle_na <- function(x, n = 1L) {
  rle_values <- na_rle_values(x)
  tunit <- time_unit(rle_values %@% "interval")
  x$values <- rle_values + sign(n) * tunit
  x
}

#' @export
na_rle_shift.mists_list_of_rle_na <- function(x, n = 1L) {
  map(x, na_rle_shift.mists_rle_na, n = n)
}

#' @export
na_rle_expand <- function(x, ...) {
  UseMethod("na_rle_expand")
}

#' @export
na_rle_expand.mists_rle_na <- function(x, ...) {
  if (is_empty(x)) {
    return(tibble("lengths" = integer(0), "values" = integer(0)))
  }
  rle_lengths <- na_rle_lengths(x)
  rle_values <- na_rle_values(x)
  tunit <- time_unit(rle_values %@% "interval")
  full_seq <- map2(rle_values, rle_lengths,
    function(.x, .y) seq(.x, by = tunit, length.out = .y))
  rep_lengths <- rep.int(rle_lengths, map_int(full_seq, vec_size))
  full_seq <- do.call("c", full_seq)
  res <- tibble("lengths" = rep_lengths, "values" = full_seq)
  interval_restore(res, x)
}

#' @export
na_rle_expand.mists_list_of_rle_na <- function(x, ...) {
  qs <- enquos(..., .named = TRUE)
  y <- eval_tidy(qs[[1]])
  stopifnot(vec_size(x) == vec_size(y))
  bind_rows(
    map2(x, y, function(.x, .y) mutate(na_rle_expand(.x), !! names(qs) := .y))
  )
}

#' @export
na_rle_table <- function(x) {
  mutate(
    count(tibble(lengths = na_rle_lengths(x)), lengths),
    nobs = n * lengths
  )
}

tbl_to_na_rle <- function(data) {
  vals <- data[["values"]]
  if (is_empty(vals)) return(na_rle()) # should also find "lengths" type

  rle_cont <- continuous_rle_impl(vals)
  add_len <- mutate(data, lengths = rep.int(cumsum(rle_cont), rle_cont))
  red_data <- summarise(group_by(add_len, lengths), values = min(values))
  red_data <- interval_restore(red_data, data)
  new_mists_rle_na(as_list(mutate(red_data, lengths = rle_cont)))
}

interval_restore <- function(x, to) {
  attr(x$values, "interval") <- attr(to$values, "interval")
  x
}

#' @importFrom dplyr intersect
#' @export
intersect.mists_rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)[, "values"]
  y_full <- na_rle_expand(y)[, "values"]
  res <- intersect(x_full, y_full) # dplyr::intersect for data frame
  tbl_to_na_rle(interval_restore(res, x))
}

#' @importFrom dplyr union
#' @export
union.mists_rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)[, "values"]
  y_full <- na_rle_expand(y)[, "values"]
  res <- arrange(union(x_full, y_full), values)
  tbl_to_na_rle(interval_restore(res, x))
}

#' @importFrom dplyr setdiff
#' @export
setdiff.mists_rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)[, "values"]
  y_full <- na_rle_expand(y)[, "values"]
  res <- setdiff(x_full, y_full)
  tbl_to_na_rle(interval_restore(res, x))
}
