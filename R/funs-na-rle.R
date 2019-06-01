#' @export
na_rle_expand <- function(x, ...) {
  UseMethod("na_rle_expand")
}

#' @export
na_rle_expand.mists_rle_na <- function(x, ...) {
  if (is_empty(x)) {
    return(dplyr::tibble("lengths" = integer(0), "values" = integer(0)))
  }
  rle_lengths <- na_rle_lengths(x)
  rle_values <- na_rle_values(x)
  tunit <- time_unit(rle_values %@% "interval")
  full_seq <- map2(rle_values, rle_lengths,
    function(.x, .y) seq(.x, by = tunit, length.out = .y))
  rep_lengths <- rep.int(rle_lengths, map_int(full_seq, vec_size))
  full_seq <- do.call("c", full_seq)
  res <- dplyr::tibble("lengths" = rep_lengths, "values" = full_seq)
  restore_interval(res, x)
}

#' @export
na_rle_expand.mists_list_of_rle_na <- function(x, ...) {
  qs <- enquos(..., .named = TRUE)
  y <- eval_tidy(qs[[1]])
  stopifnot(vec_size(x) == vec_size(y))
  dplyr::bind_rows(
    map2(x, y, function(.x, .y) mutate(na_rle_expand(.x), !! names(qs) := .y))
  )
}

#' @export
na_rle_table <- function(x) {
  mutate(
    dplyr::count(dplyr::tibble(lengths = na_rle_lengths(x)), lengths),
    nobs = n * lengths
  )
}

tbl_to_na_rle <- function(data) {
  rle_cont <- continuous_rle_impl(data[["values"]])
  add_len <- mutate(data, lengths = rep.int(cumsum(rle_cont), rle_cont))
  red_data <- summarise(group_by(add_len, lengths), values = min(values))
  red_data <- restore_interval(red_data, data)
  new_mists_rle_na(as_list(mutate(red_data, lengths = rle_cont)))
}

restore_interval <- function(new, old) {
  attr(new$values, "interval") <- attr(old$values, "interval")
  new
}

#' @importFrom dplyr intersect
#' @export
intersect.mists_rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)
  y_full <- na_rle_expand(y)
  res <- arrange(semi_join(x_full, y_full, by = "values"), values)
  tbl_to_na_rle(restore_interval(res, x))
}

#' @importFrom dplyr union
#' @export
union.mists_rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)
  y_full <- na_rle_expand(y)
  res <- 
    arrange(
      distinct(full_join(x_full, y_full, by = names(x_full)), values),
      values
    )
  tbl_to_na_rle(restore_interval(res, x))
}

#' @importFrom dplyr setdiff
#' @export
setdiff.mists_rle_na <- function(x, y, ...) {
  x_full <- na_rle_expand(x)
  y_full <- na_rle_expand(y)
  res <- arrange(anti_join(x_full, y_full, by = "values"), values)
  tbl_to_na_rle(restore_interval(res, x))
}
