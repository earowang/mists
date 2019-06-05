globalVariables(c("n_na", "pct_overall_na"))

#' Data polishing for missing values in tsibble
#'
#' @param data,before,after A tsibble.
#' @param cutoff A numeric between 0 and 1. Rows/cols will be kept, if the
#' proportion of overall missings is less than the cutoff.
#' @param na_fun Either [`na_starts_with`] or [`na_ends_with`].
#' @param tol A tolerence value as stopping rule.
#' @param quiet Report metrics along the way of the automatic polishing.
#'
#' @details
#' The proportion of overall missings is defined as the number of `NA` divided
#' by the number of **measurements** (i.e. excluding key and index).
#' @rdname mists-polish
#' @export
#' @examples
#' library(tsibble, warn.conflicts = FALSE)
#' wdi_ts <- as_tsibble(wdi, key = country_code, index = year)
#' wdi_after <- na_polish_auto(wdi_ts, cutoff = .8)
#' na_polish_metrics(wdi_ts, wdi_after)
na_polish_measures <- function(data, cutoff) {
  na_polish_assert(data, cutoff)
  prop_na_by_vars <- summarise_all(as_tibble(data), prop_overall_na)
  sel_data <- select_if(prop_na_by_vars, function(x) x < cutoff)
  select(data, !!! names(sel_data))
}

#' @rdname mists-polish
#' @export
na_polish_key <- function(data, cutoff) {
  na_polish_assert(data, cutoff)
  key_vars <- key(data)
  non_idx_data <- select(as_tibble(data), setdiff(names(data), index_var(data)))
  keyed_data <- new_grouped_df(non_idx_data, groups = key_data(data))
  add_prop_na <- 
    mutate(
      group_nest(keyed_data, .key = "pct_overall_na"),
      "pct_overall_na" := map_dbl(pct_overall_na, prop_overall_na)
    )
  key_vals <- filter(add_prop_na, pct_overall_na < cutoff)
  key_df <- select(key_vals, !!! key_vars)
  right_join(data, key_df, by = key_vars(data))
}

#' @rdname mists-polish
#' @export
na_polish_index <- function(data, cutoff, na_fun = na_starts_with) {
  na_polish_assert(data, cutoff)
  idx_len <- map_int(key_rows(data), length)
  keyed_nobs <- idx_len * NCOL(data)
  non_idx_data <- select(as_tibble(data), setdiff(names(data), index_var(data)))

  keyed_data <- new_grouped_df(data, groups = key_data(data))
  na_blocks <- summarise_all(keyed_data, na_fun)
  add_prop_na <- 
    mutate(
      group_nest(na_blocks, !!! key(data), .key = "n_na"),
      "pct_overall_na" := map_dbl(n_na, sum) / keyed_nobs,
      "n_na" := floor(pct_overall_na * idx_len)
    )
  index_pass <- filter(add_prop_na, pct_overall_na < cutoff)
  full_data <- left_join(data, index_pass, by = key_vars(data))
  grped_data <- group_by_key(full_data)
  if (is_true(all.equal(na_fun, na_starts_with))) {
    filter_data <- filter(grped_data,
      !! index(data) >= min(!! index(data)) + n_na)
  } else if (is_true(all.equal(na_fun, na_ends_with))) {
    filter_data <- filter(grped_data,
      !! index(data) <= max(!! index(data)) - n_na)
  }
  select(ungroup(filter_data), -n_na, -pct_overall_na)
}

#' @rdname mists-polish
#' @export
na_polish_index2 <- function(data, cutoff) {
  na_polish_index(data, cutoff, na_fun = na_ends_with)
}

na_polish_steps <- function() {
  list2(
    "na_polish_measures" = na_polish_measures,
    "na_polish_key" = na_polish_key,
    "na_polish_index" = na_polish_index,
    "na_polish_index2" = na_polish_index2
  )
}

#' @rdname mists-polish
#' @export
na_polish_auto <- function(data, cutoff, tol = .1, quiet = FALSE) {
  na_polish_auto_impl(data, cutoff, tol, quiet, expect = "data")
}

#' @rdname mists-polish
#' @export
na_polish_auto_trace <- function(data, cutoff, tol = .1, quiet = FALSE) {
  na_polish_auto_impl(data, cutoff, tol, quiet, expect = "report")
}

na_polish_auto_impl <- function(data, cutoff, tol = .1, quiet = FALSE,
  expect = "data") {
  stopifnot(tol >= 0 && tol <= 1)
  before <- data
  tol0 <- 1
  pass <- counter()

  lst_funs <- na_polish_steps()
  results <- list()
  while (tol0 > tol) {
    step_metrics <- # carry out individual steps to determine the order
      map_dbl(lst_funs, function(.f) {
        metrics <- na_polish_metrics(data, .f(data, cutoff = cutoff))
        metrics[[1]] * metrics[[3]]
      })

    lst_funs <- lst_funs[order(step_metrics)]
    # a full pass
    step_na <- step_removed <- double(length(lst_funs))
    for (i in seq_along(lst_funs)) {
      data0 <- data
      data <- lst_funs[[i]](data, cutoff = cutoff)
      tmp_metrics <- na_polish_metrics(data0, data)
      step_na[i] <- tmp_metrics[[1]]
      step_removed[i] <- tmp_metrics[[3]]
      step_metrics[i] <- step_na[i] * step_removed[i]
    }
    rm_funs <- step_metrics == 0 # should this be less than tol?
    lst_funs <- lst_funs[!rm_funs]
    pass_metrics <- na_polish_metrics(before, data)
    tol0 <- pass_metrics[[1]] * pass_metrics[[3]]
    before <- data
    fmt_steps <- 
      sprintf(
        "{code {fun %s}} {strong %.3f * %.3f = %.3f}", names(lst_funs), 
        step_na[!rm_funs], step_removed[!rm_funs], step_metrics[!rm_funs]
      )
    p <- pass()
    results[[p]] <- 
      tibble(
        pass = p,
        step = names(lst_funs),
        prop_na = step_na[!rm_funs],
        prop_removed = step_removed[!rm_funs],
        step_metric = step_metrics[!rm_funs],
        pass_metric = tol0
      )
    if (!quiet) {
      fmt_tol <- sprintf("{strong %.3f}", tol0)
      cli_report(p, fmt_steps, fmt_tol)
    }
  }

  if (expect == "data") {
    data
  } else {
    bind_rows(results)
  }
}

#' @rdname mists-polish
#' @export
na_polish_metrics <- function(before, after) {
  stopifnot(is_tsibble(before) && is_tsibble(after))
  stopifnot(dim(before) >= dim(after))
  stopifnot(index_var(before) == index_var(after))
  stopifnot(key_vars(before) == key_vars(after))

  mvars <- measures(before)
  bf <- select(as_tibble(before), !!! mvars)
  nobs_bf <- NROW(bf) * NCOL(bf)
  prop_na <- prop_removed <- 0
  nobs_na <- nobs_removed <- nrows_removed <- ncols_removed <- 0L
  if ((cols_rm <- NCOL(before) > NCOL(after))) { # cols removed
    removed_cols <- select(bf, setdiff(names(before), names(after)))
    nobs_removed <- NROW(removed_cols) * NCOL(removed_cols)
    nobs_na <- n_overall_na(removed_cols)
    prop_na <- prop_overall_na(removed_cols)
  }
  if ((rows_rm <- NROW(before) > NROW(after))) { # rows removed
    removed_rows <- as_tibble(anti_join(before, after, by = names(after)))
    removed_rows <- select(removed_rows, !!! mvars)
    nobs_removed <- NROW(removed_rows) * NCOL(removed_rows)
    nobs_na <- n_overall_na(removed_rows)
    prop_na <- prop_overall_na(removed_rows)
  }
  if (cols_rm && rows_rm) {
    af <- select(as_tibble(after), !!! measures(after))
    removed_rows <- # rm double counted cols part
      select(removed_rows, intersect(names(bf), names(af)))
    nobs_removed <- nobs_bf - NCOL(af) * NROW(af)
    nobs_na <- n_overall_na(removed_rows) + n_overall_na(removed_cols)
    prop_na <- nobs_na / nobs_removed
  }
  tibble(
    prop_na = prop_na,
    nobs_na = nobs_na,
    prop_removed = nobs_removed / nobs_bf,
    nobs_removed = nobs_removed,
    nrows_removed = NROW(before) - NROW(after),
    ncols_removed = NCOL(before) - NCOL(after)
  )
}

na_polish_assert <- function(data, cutoff) {
  stopifnot(is_tsibble(data) && (cutoff >= 0 && cutoff <= 1))
}

counter <- function() {
  init <- 0L
  function() {
    init <<- init + 1L
    init
  }
}

cli_report <- function(npass, step_fun, metric) {
  if (!is_installed("cliapp")) {
    abort("`quiet = FALSE` requires the cliapp packge to be installed.")
  }
  cliapp::start_app(theme = cliapp::simple_theme())
  cliapp::cli_h1(paste("Pass", npass))
  cliapp::cli_ol(step_fun)
  cliapp::cli_alert_success(metric)
}
