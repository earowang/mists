globalVariables(c("values", "group", "n", "nobs", "frac", "n.x", "overlap",
  "start", "end"))

distinct_groups <- function(x) {
  rle_cont <- continuous_rle_impl(x, tunit(x))
  rep.int(cumsum(rle_cont), rle_cont)
}

#' Range plot and spinoplot for runs of missings
#'
#' @param data A data frame that contains [`list_of_na_rle()`].
#' @inheritParams ggplot2::autoplot
#' @param ...
#' * `autoplot()`: individual aesthetics passed to `geom_line()` and `geom_point()`.
#' * `na_rle_spinoplot()`: extras passed to `facet_wrap()`.
#' @param x,y A bare variable contains [`list_of_na_rle()`] mapped to x and y.
#' @param facets A facetting variable.
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
#' na_runs_wind %>% 
#'   na_rle_rangeplot(wind_dir, origin, shape = 4)
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
  data <- 
    ungroup(mutate(
      group_by(na_rle_expand(x, y = y), y), 
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
    line_all <- c(GeomLine$aesthetics(), GeomLine$parameters(TRUE))
    point_all <- c(GeomPoint$aesthetics(), GeomPoint$parameters(TRUE))
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
#' na_runs_wind %>%
#'   na_rle_spinoplot(x = wind_dir, facets = origin)
#' na_runs_wind %>%
#'   na_rle_spinoplot(x = wind_dir, y = wind_gust, facets = origin)
#' @export
na_rle_spinoplot <- function(data, x, y = NULL, facets, ...) {
  # credits to @coolbutuseless
  # the custom breaks could not be possible without this post
  # https://coolbutuseless.github.io/2019/03/07/custom-axis-breaks-on-facetted-ggplot/
  stopifnot(is.data.frame(data))
  x <- enquo(x)
  y <- enquo(y)
  facets <- enquo(facets)
  lst_na_rle_x <- eval_tidy(x, data = data)
  facets_vals <- as.factor(eval_tidy(facets, data = data))
  
  na_runs_x <- 
    bind_rows(map2(
      lst_na_rle_x, facets_vals, 
      function(.x, .y) mutate(na_rle_table(.x), "facets" := .y)
    ))
  na_runs_x <- 
    mutate(
      group_by(na_runs_x, facets), 
      xlabs = paste(lengths, brackets(n), sep = "\n"),
      x = .5 * c(cumsum(nobs) + cumsum(dplyr::lag(nobs, default = 0)))
    )

  count_scale <- count_label <- 0
  label_x <- function(x) {
    count_label <<- count_label + 1L
    which_facet <- levels(na_runs_x$facets)[count_label]
    filter(na_runs_x, facets == which_facet)[["xlabs"]]
  }
  scale_x <- function(x) {
    count_scale <<- count_scale + 1L
    which_facet <- levels(na_runs_x$facets)[count_scale]
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
      labs(x = "runs [frequency]", y = "")
  } else {
    lst_na_rle_y <- eval_tidy(y, data = data)
    x_full <- na_rle_expand(lst_na_rle_x, facets = facets_vals)
    y_full <- na_rle_expand(lst_na_rle_y, facets = facets_vals)
    intersect_xy <- semi_join(x_full, y_full, by = c("values", "facets"))
    overlaps_xy <- count(intersect_xy, lengths, facets)
    frac_intersect <- 
      transmute(
        group_by(
          inner_join(overlaps_xy, na_runs_x, by = c("lengths", "facets")),
          lengths, facets
        ),
        "frac" := n.x / nobs, "overlap" := TRUE
      )
    frac_diff <- 
      mutate(
        group_by(frac_intersect, facets), 
        frac = 1 - frac, overlap = FALSE
      )
    frac_xy <- bind_rows(frac_intersect, frac_diff)
    grped_x <- 
        group_by(
          left_join(na_runs_x, frac_xy, by = c("lengths", "facets")), 
          facets
        )
    na_runs_xy <- 
      mutate(
        grped_x,
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
      labs(x = "runs [frequency]", y = "proportion of overlaps")
  }
}
