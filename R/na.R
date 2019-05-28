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

na_rle <- function(x) {
  if (!anyNA(x)) {
    tibble(rle = "0", n = length(x))
  } else {
    len_x <- length(x)
    na_lgl <- is.na(x)
    na_rle <- rle(na_lgl)
    lgl_rle <- na_rle$values
    na_idx <- na_rle$lengths
    na_freq <- table(na_idx[lgl_rle])
    na_freq_tbl <- tibble(rle = names(na_freq), n = as.integer(na_freq))
    new_row <- NROW(na_freq_tbl) + 1L
    na_freq_tbl[new_row, "rle"] <- "0"
    na_freq_tbl[new_row, "n"] <- sum(!na_lgl)
    na_freq_tbl
  }
}

na_starts_with <- function(x) {
  na_lgl <- is.na(x)
  if (!na_lgl[1L]) return(0L)

  rle(na_lgl)$lengths[1]
}

na_ends_with <- function(x) {
  na_lgl <- is.na(x)
  if (!na_lgl[length(na_lgl)]) return(0L)

  na_rle <- rle(na_lgl)
  na_rle$lengths[length(na_rle$lengths)]
}

n_overall_na <- function(x) {
  sum(is.na(x))
}

prop_overall_na <- function(x) {
  mean(is.na(x))
}

phi_coef <- function(...) {
  # ref: https://en.wikipedia.org/wiki/Phi_coefficient
  tab <- table(...)
  if (!all(dim(tab) == c(2, 2))) return(NaN)

  nominator <- prod(diag(tab)) - prod(c(tab[1, 2], tab[2, 1]))
  n1_row <- sum(tab[1, ])
  n2_row <- sum(tab[2, ])
  n1_col <- sum(tab[, 1])
  n2_col <- sum(tab[, 2])
  denominator <- sqrt(n1_row * n1_col * n2_row * n2_col)
  nominator / denominator
}

acf_binary <- function(x, lag_max = NULL) {
  if (is_null(lag_max)) {
    lag_max <- floor(10 * log10(length(x)))
  }
  map_dbl(seq_len(lag_max), ~ phi_coef(x, dplyr::lag(x, .x)))
}

