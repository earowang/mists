#' @export
autoplot.mists_rle_na <- function(object, ...) {
  data <- na_rle_expand(object)
  ggplot(data, aes(x = values, y = 1, group = lengths)) +
    geom_line(...) +
    geom_point(data = filter(data, lengths == 1), ...) +
    labs(x = vec_ptype_full(data$values), y = "") +
    scale_y_continuous(breaks = 1, minor_breaks = NULL) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

#' @export
autoplot.mists_list_of_rle_na <- function(object, y = seq_along(object), ...) {
  data <- mutate(na_rle_expand(object, y = y), group = paste0(y, lengths))
  ggplot(data, aes(x = values, y = y, group = group)) +
    geom_line(...) +
    geom_point(data = filter(data, lengths == 1), ...) +
    labs(x = vec_ptype_full(data$values), y = "")
}

#' @export
na_rle_spinogram <- function(x, y = NULL) {
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
    intersect_xy <- intersect(x, y)
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
