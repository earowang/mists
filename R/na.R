#' Count explicit NA
#' 
#' @param .data A `tbl_ts`.
#' @param ... A bare variable that may contain `NA`.
#'
#' @export
#' @return
#' A tibble contains:
#' * the "key" of the `tbl_ts`
#' * ".from": the starting time point of `NA`
#' * ".to": the ending time point of `NA`
#' * ".n": the number of `NA` during the time period
#' @examples
#' pedestrian %>% 
#'   fill_gaps(.full = TRUE) %>% 
#'   count_na(Count)
count_na <- function(.data, ...) {
  exprs <- enexprs(...)
  if (is_false(has_length(exprs, 1))) {
    abort("`count_na()` only accepts one variable.")
  }
  idx <- index(.data)
  grped_tbl <- group_by(as_tibble(.data), !!! key(.data))
  lst_out <- summarise(grped_tbl, na = list2(tbl_na(!!! exprs, !! idx)))
  idx_type <- class(lst_out[["na"]][[1]][[".from"]])
  out <- unnest(lst_out, na)
  class(out[[".from"]]) <- class(out[[".to"]]) <- idx_type
  tibble(!!! out)
}

tbl_na <- function(x, y) {
  if (!anyNA(x)) {
    tibble(.from = y[0], .to = y[0], .n = integer())
  } else {
    len_x <- length(x)
    na_lgl <- is.na(x)
    na_rle <- rle(na_lgl)
    lgl_rle <- na_rle$values
    na_idx <- na_rle$lengths
    to <- cumsum(na_idx)
    from <- c(1, to[-length(to)] + 1)
    na_nobs <- na_idx[lgl_rle]
    tibble(
      .from = y[from][lgl_rle],
      .to = y[to][lgl_rle],
      .n = na_nobs
    )
  }
}
