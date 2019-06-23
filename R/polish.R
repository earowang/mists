globalVariables(c("n_na", "pct_overall_na"))

#' Missing data polishing for tsibble
#'
#' If the proportion of overall missings is less than the cutoff,
#' * `na_polish_measures()` removes columns or observations.
#' * `na_polish_key()` polishes data by rows or observations, removing the whole
#' rows of key series.
#' * `na_polish_index()` polishes data by rows or observations, removing either
#' the starting or the ending `NA` blocks (if any) within each key series.
#' * `na_polish_index2()` polishes data by rows or observations, removing the
#' ending `NA` blocks (if any) within each key series. It is a shortcut of
#' `na_polish_index(na_fun = na_ends_with)`.
#'
#' @param data A tsibble.
#' @param cutoff A numeric between 0 and 1. Rows/cols will be kept, if the
#' proportion of overall missings is less than the cutoff.
#' @param na_fun Either [`na_starts_with`] or [`na_ends_with`].
#'
#' @family missing value polishing functions
#' @details
#' The proportion of overall missings is defined as the number of `NA` divided
#' by the number of **measurements** (i.e. excluding key and index).
#' @rdname mists-polish
#' @examples
#' wdi_ts <- tsibble::as_tsibble(wdi, key = country_code, index = year)
#' (wdi_cols <- na_polish_measures(wdi_ts, cutoff = .7))
#' # columns removed
#' setdiff(names(wdi_ts), names(wdi_cols))
#' @export
na_polish_measures <- function(data, cutoff) {
  na_polish_assert(data, cutoff)
  prop_na_by_vars <- summarise_all(as_tibble(data), prop_overall_na)
  sel_data <- select_if(prop_na_by_vars, function(x) x < cutoff)
  select(data, !!! names(sel_data))
}

#' @rdname mists-polish
#' @examples
#' na_polish_key(wdi_ts, cutoff = .7)
#' @export
na_polish_key <- function(data, cutoff) {
  na_polish_assert(data, cutoff)
  key_vars <- key(data)
  if (is_empty(key_vars)) return(data)
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
#' @examples
#' na_polish_index(wdi_ts, cutoff = .7)
#' @export
na_polish_index <- function(data, cutoff, na_fun = na_starts_with) {
  na_polish_assert(data, cutoff)
  idx_len <- map_int(key_rows(data), length)
  keyed_nobs <- idx_len * NCOL(data)
  non_idx_data <- select(as_tibble(data), setdiff(names(data), index_var(data)))

  keyed_data <- new_grouped_df(data, groups = key_data(data))
  na_blocks <- group_by(summarise_all(keyed_data, na_fun), !!! key(data))
  add_prop_na <- 
    mutate(
      group_nest(na_blocks, .key = "n_na"),
      "pct_overall_na" := map_dbl(n_na, sum) / keyed_nobs,
      "n_na" := floor(pct_overall_na * idx_len)
    )
  index_pass <- filter(add_prop_na, pct_overall_na < cutoff)
  key_vars <- key_vars(data)
  if (is_empty(key_vars)) {
    full_data <- mutate(data, !!! index_pass)
  } else {
    full_data <- left_join(data, index_pass, by = key_vars)
  }
  grped_data <- group_by_key(full_data)
  if (is_true(all.equal(na_fun, na_starts_with))) {
    filter_data <- filter(grped_data,
      !! index(data) >= min(!! index(data)) + n_na)
  } else if (is_true(all.equal(na_fun, na_ends_with))) {
    filter_data <- filter(grped_data,
      !! index(data) <= max(!! index(data)) - n_na)
  } else {
    abort("`na_fun` requires either `na_starts_with` or `na_ends_with`.")
  }
  select(ungroup(filter_data), -n_na, -pct_overall_na)
}

#' @rdname mists-polish
#' @examples
#' na_polish_index2(wdi_ts, cutoff = .7)
#' @export
na_polish_index2 <- function(data, cutoff) {
  na_polish_index(data, cutoff, na_fun = na_ends_with)
}

#' Automate missing data polishing for tsibble
#'
#' It is an iterative process for minimising the loss until a tolerance value.
#' * `na_polish_auto()` returns the polished data.
#' * `na_polish_autotrace()` returns a tibble for documenting the steps and metrics.
#'
#' @inheritParams na_polish_measures
#' @param tol A tolerance value close or equal to zero as stopping rule. It
#' compares to the loss defined as `prop_na * prop_removed` to be minimised.
#' See [`na_polish_metrics()`] for details.
#' @param funs A list of `na_polish_*()` functions to go through.
#' @param quiet If `FALSE`, report metrics at each step and pass of the polishing
#' process. It requires the "cliapp" package to be installed.
#'
#' @family missing value polishing functions
#' @rdname mists-polish-auto
#' @export
#' @examples
#' \dontrun{
#' wdi_ts <- tsibble::as_tsibble(wdi, key = country_code, index = year)
#' wdi_after <- na_polish_auto(wdi_ts, cutoff = .8)
#' na_polish_metrics(wdi_ts, wdi_after)
#'
#' # Trace down `na_polish_auto()`
#' na_polish_autotrace(wdi_ts, cutoff = .8, quiet = TRUE)
#' }
na_polish_auto <- function(data, cutoff, tol = .1, funs = na_polish_funs(),
  quiet = FALSE) {
  na_polish_auto_impl(data, cutoff, tol, funs, quiet, expect = "data")
}

#' @rdname mists-polish-auto
#' @export
na_polish_autotrace <- function(data, cutoff, tol = .1, funs = na_polish_funs(),
  quiet = FALSE) {
  na_polish_auto_impl(data, cutoff, tol, funs, quiet, expect = "report")
}

na_polish_auto_impl <- function(data, cutoff, tol = .1, funs = na_polish_funs(),
  quiet = FALSE, expect = "data") {
  stopifnot(tol >= 0 && tol <= 1)
  before <- data
  tol0 <- 1
  pass <- counter()

  lst_funs <- funs
  results <- list()
  while (tol0 > tol) {
    step_metrics <- # carry out individual steps to determine the order
      map_dbl(lst_funs, function(.f) {
        metrics <- na_polish_metrics(data, .f(data, cutoff = cutoff))
        metrics[["prop_na"]] * metrics[["prop_removed"]]
      })

    lst_funs <- lst_funs[order(step_metrics, decreasing = TRUE)]
    # a full pass
    step_na <- step_removed <- double(length(lst_funs))
    for (i in seq_along(lst_funs)) {
      data0 <- data
      data <- lst_funs[[i]](data, cutoff = cutoff)
      tmp_metrics <- na_polish_metrics(data0, data)
      step_na[i] <- tmp_metrics[["prop_na"]]
      step_removed[i] <- tmp_metrics[["prop_removed"]]
      step_metrics[i] <- step_na[i] * step_removed[i]
    }
    rm_funs <- step_metrics == 0 # should this be less than tol?
    lst_funs <- lst_funs[!rm_funs]
    pass_metrics <- na_polish_metrics(before, data)
    tol0 <- pass_metrics[["prop_na"]] * pass_metrics[["prop_removed"]]
    before <- data
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

    if (!quiet) cli_report(p, results[[p]])
  }

  if (expect == "data") {
    data
  } else {
    bind_rows(results)
  }
}

#' @rdname mists-polish-auto
#' @keywords internal
#' @usage NULL
#' @export
na_polish_funs <- function() {
  list(
    "na_polish_measures" = na_polish_measures,
    "na_polish_key" = na_polish_key,
    "na_polish_index" = na_polish_index,
    "na_polish_index2" = na_polish_index2
  )
}

#' Report metrics for missing data polishing
#'
#' @param before,after Tsibbles before and after polishing.
#' @return
#' A tibble contains:
#' * `prop_na` & `nobs_na`: The proportion of `NA`s in the sliced data
#' (the difference between `before` and `after`).
#' * `prop_removed`, `nobs_removed`, `nrows_removed`, & `ncols_removed`: The
#' proportion of removed observations over the overall observations.
#' @family missing value polishing functions
#' @details
#' The metric used for measuring the effect of polishing events is
#' `prop_na * prop_removed`. We'd like to minimise the loss by minimising both
#' `prop_na` and `prop_removed` over sequential polishing events.
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
  if (NROW(data) == 0L) {
    abort("`data` can't be empty.")
  }
  if (!is_tsibble(data)) {
    abort("`data` requires a tsibble object.")
  }
  if (cutoff < 0 || cutoff > 1) {
    abort("`cutoff` requires a numeric between 0 and 1.")
  }
}

counter <- function() {
  init <- 0L
  function() {
    init <<- init + 1L
    init
  }
}

cli_report <- function(npass, tbl) {
  if (!is_installed("cliapp")) {
    abort("`quiet = FALSE` requires the \"cliapp\" packge to be installed.")
  }
  if (vec_size(tbl) == 0) return()
  fmt_steps <- 
    sprintf(
      "{arg %s} {emph %.3f * %.3f = %.3f}", 
      justify(
        backticks(paste0(tbl[["step"]], parenthesis(""))),
        right = FALSE, space = "\u00a0"
      ), 
      tbl[["prop_na"]], tbl[["prop_removed"]], tbl[["step_metric"]]
    )
  fmt_tol <- sprintf("{emph %.3f}", unique(tbl[["pass_metric"]]))
  cliapp::start_app(theme = cliapp::simple_theme())
  cliapp::cli_div(theme = list(span.emph = list(color = "red")))
  cliapp::cli_h1(paste("Pass", npass))
  cliapp::cli_ol(fmt_steps)
  cliapp::cli_alert_success(fmt_tol)
}
