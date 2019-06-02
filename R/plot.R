globalVariables(c("values", "group", "n", "nobs", "frac", "n.x", "overlap"))

distinct_groups <- function(x) {
  rle_cont <- continuous_rle_impl(x, tunit(x))
  rep.int(cumsum(rle_cont), rle_cont)
}

#' Range plot and spinogram for runs of missings
#'
#' @inheritParams ggplot2::autoplot
#' @param ... Individual aesthetics passed to `geom_line()` and `geom_point()`.
#' @param x,y Objects returned by [`na_rle()`].
#'
#' @rdname mists-plot
#' @examples
#' if (!requireNamespace("nycflights13", quietly = TRUE)) {
#'   stop("Please install the nycflights13 package to run these following examples.")
#' }
#' library(dplyr, warn.conflicts = FALSE)
#' na_runs_wind <- nycflights13::weather %>% 
#'   group_by(origin) %>% 
#'   summarise_at(vars(contains("wind")), ~ list_of_na_rle(., time_hour))
#' 
#' autoplot(na_runs_wind$wind_dir[[1]])
#' autoplot(na_runs_wind$wind_dir, na_runs_wind$origin)
#' autoplot(
#'   vctrs::as_list_of(na_runs_wind$wind_dir[[2]], na_runs_wind$wind_gust[[2]]),
#'   y = paste(c("wind_dir", "wind_gust"), "JFK", sep = "@"),
#'   size = 2.5
#' )
#' @method autoplot mists_rle_na
#' @export
autoplot.mists_rle_na <- function(object, ...) {
  data <- mutate(na_rle_expand(object), "group" := distinct_groups(values))
  ggplot(data, aes(x = values, y = 1, group = group)) +
    geom_line(...) +
    geom_point(data = filter(data, lengths == 1), shape = 4, ...) +
    labs(x = vec_ptype_full(data$values), y = "") +
    scale_y_continuous(breaks = 1, minor_breaks = NULL) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

#' @method autoplot mists_list_of_rle_na
#' @export
autoplot.mists_list_of_rle_na <- function(object, y = seq_along(object), ...) {
  data <- 
    ungroup(mutate(
      group_by(na_rle_expand(object, y = y), y), 
      "group" := paste(y, distinct_groups(values), sep = "-")
    ))
  ggplot(data, aes(x = values, y = y, group = group)) +
    geom_line(...) +
    geom_point(data = filter(data, lengths == 1), shape = 4, ...) +
    labs(x = vec_ptype_full(data$values), y = "")
}

#' @rdname mists-plot
#' @examples
#' na_rle_spinogram(na_runs_wind$wind_dir[[1]])
#' na_rle_spinogram(na_runs_wind$wind_dir[[1]], na_runs_wind$wind_gust[[1]])
#' na_rle_spinogram(na_runs_wind$wind_dir[[1]], na_runs_wind$wind_dir[[3]])
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
    x_full <- na_rle_expand(x)
    y_full <- na_rle_expand(y)
    intersect_xy <- semi_join(x_full, y_full, by = "values")
    overlaps_xy <- count(intersect_xy, lengths)
    frac_intersect <- 
      transmute(
        inner_join(overlaps_xy, na_runs_x, by = "lengths"),
        lengths, "frac" := n.x / nobs, "overlap" := TRUE
      )
    frac_diff <- mutate(frac_intersect, frac = 1 - frac, overlap = FALSE)
    frac_xy <- bind_rows(frac_intersect, frac_diff)
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
