polish_cols_measures <- function(data, cutoff) {
  stopifnot(is_tsibble(data))
  prop_na_by_vars <- summarise_all(as_tibble(data), prop_overall_na)
  sel_data <- select_if(prop_na_by_vars, function(x) x < cutoff)
  select(data, !!! names(sel_data))
}

polish_rows_key <- function(data, cutoff) {
  key_vars <- key(data)
  nest_tbl <- group_nest(data, !!! key_vars, .key = "pct_overall_na")
  add_prop_na <- 
    mutate(
      nest_tbl, 
      "pct_overall_na" := map_dbl(pct_overall_na, prop_overall_na)
    )
  key_vals <- filter(add_prop_na, pct_overall_na < cutoff)
  key_df <- select(key_vals, !!! key_vars)
  right_join(data, key_df, by = key_vars(data))
}

polish_rows_index <- function(data, na_fun = na_starts_with) {
  idx_len <- map_int(key_rows(data), length)
  keyed_nobs <- idx_len * NCOL(data)
  
  keyed_data <- new_grouped_df(data, groups = key_rows(data))
  na_blocks <- summarise_all(keyed_data, na_fun)
  index_pass <- 
    mutate(
      group_nest(na_blocks, !!! key(data), .key = "..n_na"),
      ..n_na = floor(map_dbl(..n_na, sum) / keyed_nobs * idx_len)
    )
  full_data <- left_join(data, index_pass, by = key_vars(data))
  filter_data <- 
    filter(
      group_by_key(full_data),
      !! index(data) >= min(!! index(data)) + ..n_na
    )
  select(ungroup(filter_data), -..n_na)
}

polish_metrics <- function(before, after) {
  stopifnot(dim(before) >= dim(after))
  before <- as_tibble(before)
  before_nobs <- NROW(before) * NCOL(before)
  if ((cols_rm <- NCOL(before) > NCOL(after))) { # cols removed
    removed_cols <- select(before, setdiff(names(before), names(after)))
    removed_nobs <- NROW(removed_cols) * NCOL(removed_cols)
    prop_na <- prop_overall_na(removed_cols)
  }
  if ((rows_rm <- NROW(before) > NROW(after))) { # rows removed
    removed_rows <- anti_join(before, after, by = names(after))
    removed_nobs <- NROW(removed_rows) * NCOL(removed_rows)
    prop_na <- prop_overall_na(removed_rows)
  }
  if (cols_rm && rows_rm) {
    removed_rows <- select(removed_rows, intersect(names(before), names(after)))
    removed_nobs <- before_nobs - NCOL(after) * NROW(after)
    n_na <- n_overall_na(removed_rows) + n_overall_na(removed_cols)
    prop_na <- n_na / removed_nobs
  }
  c(prop_na = prop_na, prop_removed = removed_nobs / before_nobs)
}