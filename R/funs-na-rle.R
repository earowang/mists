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
    count(tibble(lengths = na_rle_lengths(x)), lengths),
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

#' @export
na_rle_ggspinogram <- function(x, y = NULL) {
  na_runs_x <- na_rle_table(x)
  na_runs_x <- 
    mutate(
      na_runs_x, 
      xlabs = paste(lengths, brackets(n), sep = "\n"),
      x = .5 * c(cumsum(nobs) + cumsum(dplyr::lag(nobs, default = 0)))
    )
  if (is_null(y)) {
    ggplot(na_runs_x, aes(x = x, y = 1, width = nobs)) +
      geom_bar(stat = "identity", colour = "white") +
      scale_x_continuous(
        labels = na_runs_x$xlabs,
        breaks = na_runs_x$x,
        minor_breaks = NULL
      ) +
      labs(x = "runs [frequency]", y = "")
  } else {
    intersect_xy <- intersect.mist_rle_na(x, y)
    overlaps_xy <- dplyr::count(intersect_xy, lengths)
    frac_intersect <- 
      transmute(
        inner_join(overlaps_xy, na_runs_x, by = "lengths"),
        lengths, frac = n.x / nobs, overlap = TRUE
      )
    frac_diff <- mutate(frac_intersect, frac = 1 - frac, overlap = FALSE)
    frac_xy <- dplyr::bind_rows(frac_intersect, frac_diff)
    na_runs_xy <- mutate(
      left_join(na_runs_x, frac_xy, by = "lengths"),
      overlap = ifelse(is.na(frac), FALSE, overlap),
      frac = ifelse(is.na(frac), 1, frac)
    )
    ggplot(na_runs_xy, aes(x = x, y = frac, width = nobs, fill = overlap)) +
      geom_bar(stat = "identity", colour = "white") +
      scale_x_continuous(
        labels = na_runs_x$xlabs,
        breaks = na_runs_x$x,
        minor_breaks = NULL
      ) +
      labs(x = "runs [frequency]", y = "proportion of overlaps")
  }
}
