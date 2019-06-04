globalVariables(c("values", "group", "n", "nobs", "frac", "n.x", "overlap",
  "start", "end"))

distinct_groups <- function(x) {
  rle_cont <- continuous_rle_impl(x, tunit(x))
  rep.int(cumsum(rle_cont), rle_cont)
}

#' Range plot and spinoplot for runs of missings
#'
#' @inheritParams ggplot2::autoplot
#' @param ... Individual aesthetics passed to `geom_line()` and `geom_point()`.
#' @param x,y Objects returned by [`na_rle()`].
#'
#' @name mists-plot
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
#'   y = paste(c("wind_dir", "wind_gust"), "JFK", sep = "@")
#' )
#' @method autoplot mists_rle_na
#' @export
autoplot.mists_rle_na <- function(object, y = as.factor(1L), ...) {
  autoplot.mists_list_of_rle_na(as_list_of(object), y = y, ...)
}

#' @method autoplot mists_list_of_rle_na
#' @export
autoplot.mists_list_of_rle_na <- function(object, y = seq_along(object), ...) {
  data <- 
    ungroup(mutate(
      group_by(na_rle_expand(object, y = y), y), 
      "group" := paste(y, distinct_groups(values), sep = "-")
    ))
  ends <- 
    summarise(
      group_by(data, y, group), 
      "start" := min(values), "end" := max(values)
    )
  unique_grp <- filter(count(data, group), n == 1)[["group"]]
  rngs <- filter(data, !(group %in% unique_grp))

  # separate params for geom_line and geom_point from ...
  params_list <- list2(...)
  if (has_length(params_list, 0)) {
    line_params <- point_params <- list2()
  } else {
    names_params <- names(params_list)
    line_all <- c(GeomLine$aesthetics(), GeomLine$paramseters(TRUE))
    point_all <- c(GeomPoint$aesthetics(), GeomPoint$paramseters(TRUE))
    line_params <- params_list[which(names_params %in% line_all)]
    point_params <- params_list[which(names_params %in% point_all)]
  }

  plot <- ggplot()
  line_params$data <- rngs
  line_params$mapping <- aes(x = values, y = y, group = group)
  line_params$inherit.aes <- FALSE
  plot <- plot + do.call(geom_line, line_params)

  point_params$data <- ends
  point_params$inherit.aes <- FALSE
  point_params$mapping <- aes(x = start, y = y)
  plot <- plot + do.call(geom_point, point_params)
  point_params$mapping <- aes(x = end, y = y)
  plot <- plot + do.call(geom_point, point_params)
  plot + labs(x = vec_ptype_full(data$values), y = "")
}

#' @rdname mists-plot
#' @examples
#' na_rle_spinoplot(na_runs_wind$wind_dir[[1]])
#' na_rle_spinoplot(na_runs_wind$wind_dir[[1]], na_runs_wind$wind_gust[[1]])
#' na_rle_spinoplot(na_runs_wind$wind_dir[[1]], na_runs_wind$wind_dir[[3]])
#' @export
na_rle_spinoplot <- function(x, y = NULL) {
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
