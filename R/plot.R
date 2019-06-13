globalVariables(c("indices", "group", "n", "nobs", "frac", "n.x", "overlap",
  "start", "end", "x"))

#' Range plot and spinoplot for runs of missings
#'
#' `na_rle_spineplot()` is an alias of `na_rle_spinoplot()`.
#'
#' @param data A data frame that contains [`list_of_na_rle()`].
#' @param x,y A bare variable contains [`list_of_na_rle()`] mapped to x and y.
#' @param facets A facetting variable.
#' @inheritParams ggplot2::autoplot
#' @param ...
#' * `na_rle_rangeplot()`: passed to `geom_segment()` & `geom_point()`.
#' * `na_rle_spinoplot()`: passed to `facet_wrap()`.
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
#' na_runs_wind %>% 
#'   na_rle_rangeplot(wind_dir, origin)
#' # autoplot() method
#' autoplot(na_runs_wind$wind_gust, y = na_runs_wind$origin)
#' @export
na_rle_rangeplot <- function(data, x, y = NULL, ...) {
  stopifnot(is.data.frame(data))
  x <- eval_tidy(enquo(x), data = data)
  y <- enquo(y)
  if (quo_is_null(y)) {
    y <- as.factor(seq_len(vec_size(data)))
  } else {
    y <- eval_tidy(y, data = data)
  }
  tbl_rng <- 
    tibble(
      "start" := start(x),
      "end" := end(x),
      "y" := rep(y, times = map_int(x, length))
    )
  data <- 
    ungroup(mutate(
      group_by(tbl_rng, y), 
      "group" := paste(y, n(), sep = "-")
    ))

  # separate params for geom_segment and geom_point from ...
  params_list <- list2(...)
  if (has_length(params_list, 0)) {
    segment_params <- point_params <- list2()
  } else {
    names_params <- names(params_list)
    segment_all <- c(GeomSegment$aesthetics(), GeomSegment$parameters(TRUE))
    point_all <- c(GeomPoint$aesthetics(), GeomPoint$parameters(TRUE))
    segment_params <- params_list[which(names_params %in% segment_all)]
    point_params <- params_list[which(names_params %in% point_all)]
  }

	plot <- ggplot()
  segment_params$data <- data
  segment_params$mapping <- 
    aes(x = start, xend = end, y = y, yend = y, group = group)
  segment_params$inherit.aes <- FALSE
  plot <- plot + do.call(geom_segment, segment_params)

  point_params$data <- data
  point_params$inherit.aes <- FALSE
  point_params$mapping <- aes(x = start, y = y)
  plot <- plot + do.call(geom_point, point_params)
  point_params$mapping <- aes(x = end, y = y)
  plot <- plot + do.call(geom_point, point_params)
  plot + labs(x = vec_ptype_full(data$start), y = "")
}

#' @rdname mists-plot
#' @examples
#' na_runs_wind %>%
#'   na_rle_spinoplot(x = wind_dir, facets = origin)
#' na_runs_wind %>%
#'   na_rle_spinoplot(x = wind_dir, y = wind_gust, facets = origin)
#' @export
na_rle_spinoplot <- function(data, x, y = NULL, facets = NULL, ...) {
  # credits to @coolbutuseless
  # the custom breaks could not be possible without this post
  # https://coolbutuseless.github.io/2019/03/07/custom-axis-breaks-on-facetted-ggplot/
  stopifnot(is.data.frame(data))
  x <- enquo(x)
  y <- enquo(y)
  facets <- enquo(facets)
  lst_na_rle_x <- eval_tidy(x, data = data)
  if (quo_is_null(facets)) {
    facets_vals <- as.factor(vec_seq_along(lst_na_rle_x))
  } else {
    facets_vals <- as.factor(eval_tidy(facets, data = data))
  }
  xlab <- paste("runs [frequency]", parenthesis(as_label(x)))
  
  na_runs_lst_x <- 
    map2(
      lst_na_rle_x, facets_vals, 
      function(.x, .y) mutate(na_rle_table(.x), "facets" := .y)
    )
  na_runs_x <- vec_rbind(!!! na_runs_lst_x)
  na_runs_x <- 
    mutate(
      group_by(na_runs_x, facets), 
      xlabs = paste(lengths, brackets(n), sep = "\n"),
      x = .5 * c(cumsum(nobs) + cumsum(dplyr::lag(nobs, default = 0)))
    )

  count_label <- counter()
  count_scale <- counter()
  label_x <- function(x) {
    which_facet <- levels(na_runs_x$facets)[count_label()]
    filter(na_runs_x, facets == which_facet)[["xlabs"]]
  }
  scale_x <- function(x) {
    which_facet <- levels(na_runs_x$facets)[count_scale()]
    filter(na_runs_x, facets == which_facet)[["x"]]
  }
  if (quo_is_null(y)) {
    ggplot(na_runs_x, aes(x = x, y = 1, width = nobs)) +
      geom_bar(stat = "identity", colour = "white") +
      scale_x_continuous(
        labels = label_x,
        breaks = scale_x,
        minor_breaks = NULL
      ) +
      facet_wrap(~ facets, scales = 'free_x', ...) + 
      labs(x = xlab, y = "")
  } else {
    lst_na_rle_y <- eval_tidy(y, data = data)
    ylab <- paste("proportion of overlaps", parenthesis(as_label(y)))
    x_full <- na_rle_expand(lst_na_rle_x, facets = facets_vals)
    y_full <- na_rle_expand(lst_na_rle_y, facets = facets_vals)
    intersect_xy <- semi_join(x_full, y_full, by = c("indices", "facets"))
    overlaps_xy <- count(intersect_xy, lengths, facets)
    inner_xy <- inner_join(overlaps_xy, na_runs_x, by = c("lengths", "facets"))
    frac_intersect <- 
      transmute(
        group_by(inner_xy, lengths, facets),
        "frac" := n.x / nobs, "overlap" := TRUE
      )
    frac_diff <- 
      mutate(
        group_by(frac_intersect, facets), 
        "frac" := 1 - frac, "overlap" := FALSE
      )
    frac_xy <- vec_rbind(frac_intersect, frac_diff)
    ljoin_xy <- left_join(na_runs_x, frac_xy, by = c("lengths", "facets"))
    na_runs_xy <- 
      mutate(
        group_by(ljoin_xy, facets),
        "overlap" := ifelse(is.na(frac), FALSE, overlap),
        "frac" := ifelse(is.na(frac), 1, frac)
      )
    ggplot(na_runs_xy, aes(x = x, y = frac, width = nobs, fill = overlap)) +
      geom_bar(stat = "identity", colour = "white") +
      scale_x_continuous(
        labels = label_x,
        breaks = scale_x,
        minor_breaks = NULL
      ) +
      facet_wrap(~ facets, scales = 'free_x', ...) + 
      labs(x = xlab, y = ylab)
  }
}

#' @rdname mists-plot
#' @export
#' @usage NULL
na_rle_spineplot <- na_rle_spinoplot

#' @rdname mists-plot
#' @method autoplot rle_na
#' @export
autoplot.rle_na <- function(object, y = as.factor(1L), ...) {
  autoplot.list_of_rle_na(as_list_of(object), y = y, ...)
}

#' @method autoplot list_of_rle_na
#' @export
autoplot.list_of_rle_na <- function(object, y = seq_along(object), ...) {
  data <- tibble("x" := object, "y" := y)
  na_rle_rangeplot(data, x = x, y = y)
}

#' Add a layer of `list_of_na_rle()` to an existing ggplot
#'
#' @param var A variable that contains `list_of_na_rle()`.
#' @param data A data frame.
#' @param ... Passed to `geom_rect()`.
#' @examples
#' if (!requireNamespace("nycflights13", quietly = TRUE)) {
#'   stop("Please install the nycflights13 package to run these following examples.")
#' }
#' library(dplyr, warn.conflicts = FALSE)
#' weather <- nycflights13::weather
#' na_runs_wind <- weather %>% 
#'   group_by(origin) %>% 
#'   summarise_at(
#'     vars(contains("wind")), 
#'     ~ list_of_na_rle(., index_by = time_hour)
#'   )
#' 
#' library(ggplot2)
#' ggplot(weather, aes(x = time_hour, y = wind_gust)) +
#'   geom_line() +
#'   layer_na_rle(wind_gust, data = na_runs_wind, fill = "red", alpha = .5) +
#'   facet_grid(origin ~ .)
#' @export
layer_na_rle <- function(var, data, ...) {
  var_eval <- eval_tidy(enquo(var), data)
  if (!is_list_of_rle_na(var_eval)) {
    abort("`var` must be an object of `list_of_na_rle()`.")
  }
  cols_non_na_rle <- names(data[!map_lgl(data, is_list_of_rle_na)])
  data <- data[cols_non_na_rle]
  na_rle_df <- 
    tibble(
      "start" := start(var_eval) - common_tunit(na_rle_indices(var_eval)),
      "end" := end(var_eval)
    )
  base_idx <- vec_seq_along(data)
  rep_times <- map_int(var_eval, length)
  df_idx <- rep.int(base_idx, times = rep_times)
  data <- data[df_idx, ]
  data <- vec_cbind(data, na_rle_df)
  geom_rect(
    aes(xmin = start, xmax = end), ymin = -Inf, ymax = Inf, 
    data = data, ..., inherit.aes = FALSE
  )
}
