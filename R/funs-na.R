#' Handlers for missing values
#'
#' @param x A vector.
#'
#' @rdname mists-na-helpers
#' @examples
#' x <- c(rep(NA, 4), 10:6, NA, 4:1, NA)
#' na_starts_with(x)
#' @export
na_starts_with <- function(x) {
  na_rle <- na_rle_impl(x)
  if (is_rle_empty(na_rle)) return(0L)

  if (na_rle[["values"]][1L]) {
    na_rle[["lengths"]][1L]
  } else {
    0L
  }
}

#' @rdname mists-na-helpers
#' @examples
#' na_ends_with(x)
#' @export
na_ends_with <- function(x) {
  na_rle <- na_rle_impl(x)
  if (is_rle_empty(na_rle)) return(0L)

  if (tail(na_rle[["values"]], 1L)) {
    tail(na_rle[["lengths"]], 1L)
  } else {
    0L
  }
}

#' @rdname mists-na-helpers
#' @examples
#' na_elsewhere(x)
#' @export
na_elsewhere <- function(x) {
  na_rle <- na_rle_impl(x)
  if (is_rle_empty(na_rle)) return(0L)

  if (head(na_rle[["values"]], 1L)) {
    na_starts <- head(na_rle[["lenghts"]], 1L)
  } else {
    na_starts <- 0L
  }
  if (tail(na_rle[["values"]], 1L)) {
    na_ends <- tail(na_rle[["lengths"]], 1L)
  } else {
    na_ends <- 0L
  }
  na_ttl <- sum(na_rle[["lengths"]][na_rle[["values"]]])
  na_ttl - na_starts - na_ends
}

is_rle_empty <- function(x) {
  is_empty(x[["values"]])
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

na_acf <- function(x, lag_max = NULL) {
  x <- is.na(x)
  if (is_null(lag_max)) {
    lag_max <- floor(10 * log10(length(x)))
  }
  seq_lag <- seq_len(lag_max)
  res <- map_dbl(seq_lag, function(.x) phi_coef(x, dplyr::lag(x, .x)))
  names(res) <- seq_lag
  res
}
