## ---- load
library(tidyverse)
library(lubridate)
library(tsibble)
library(imputeTS)
library(mists)
library(rlang)
library(sugrrants)
library(feasts)
library(patchwork)
set.seed(2019)

ts_airgap <- as_tsibble(tsAirgap)
ts_NH4 <- as_tsibble(tsNH4) %>%
  mutate(
    index = seq(ymd_hm("2010-11-30 16:10"), by = "10 min", length.out = 4552)
  )
ts_heating <- as_tsibble(tsHeating) %>%
  mutate(
    index = seq(ymd_hm("2013-11-18 05:12"), by = "1 min", length.out = 606837)
  )

ts_airgap_full <- as_tsibble(tsAirgapComplete)

# Types of temporal missings
miss_at_regular <- function(.data) {
  idx <- eval_tidy(index(.data), .data)
  rnd_seas <- month(sample(idx, size = 1))
  mutate(
    .data,
    value = ifelse(month(idx) %in% rnd_seas, NA, value)
  )
}

miss_at_occasional <- function(.data) {
  idx <- eval_tidy(index(.data), .data)
  rnd_idx <- sample(idx, size = round(NROW(.data) * 0.03))
  mutate(
    .data,
    value = ifelse(idx %in% rnd_idx, NA, value)
  )
}

miss_at_trend <- function(.data) {
  idx <- eval_tidy(index(.data), .data)
  init <- floor(NROW(.data) * 0.1)
  rnd_start <- idx[init]
  trend_idx <- idx[cumsum(seq_len(init)) + init]
  mutate(
    .data,
    value = ifelse(idx %in% trend_idx, NA, value)
  )
}

miss_at_runs <- function(.data) {
  idx <- eval_tidy(index(.data), .data)
  rnd_init <- sample(idx, size = ceiling(NROW(.data) * 0.01))
  rnd_len <- sample(5:(NROW(.data) * 0.1), size = length(rnd_init))
  rnd_run <- do.call("c", purrr::map2(rnd_init, rnd_len, ~ .x + seq_len(.y)))
  mutate(
    .data,
    value = ifelse(idx %in% rnd_run, NA, value)
  )
}

# miss_at_cycle <- function(.data) {
#
# }

## ---- miss-types
ts_airgap_trend <- miss_at_trend(ts_airgap_full) %>%
  mutate(type = "missing at trend") %>%
  update_tsibble(key = id(type))
ts_airgap_regular <- miss_at_regular(ts_airgap_full) %>%
  mutate(type = "missing at regular") %>%
  update_tsibble(key = id(type))
ts_airgap_occasional <- miss_at_occasional(ts_airgap_full) %>%
  mutate(type = "missing at occasional") %>%
  update_tsibble(key = id(type))
ts_airgap_runs <- miss_at_runs(ts_airgap_full) %>%
  mutate(type = "missing at runs") %>%
  update_tsibble(key = id(type))
ts_airgap_miss <-
  rbind(
    ts_airgap_trend, ts_airgap_regular,
    ts_airgap_occasional, ts_airgap_runs
  )

ggplot() +
  geom_line(aes(x = index, y = value), data = ts_airgap_miss) +
  geom_rect(
    aes(xmin = .from - 1, xmax = .to + 1), ymin = -Inf, ymax = Inf,
    data = count_na(ts_airgap_miss, value), colour = "grey50", fill = "grey50" #, alpha = 0.2
  ) +
  facet_wrap(~ type, ncol = 1) +
  theme_bw()

## ---- miss-types-acf
library(vcd)
ts_airgap_dummy <- ts_airgap_miss %>%
  mutate(dummy = ifelse(is.na(value), 1L, 0L))

phi_coef <- function(...) {
  # ref: https://en.wikipedia.org/wiki/Phi_coefficient
  tab <- table(...)
  stopifnot(all(dim(tab) == c(2, 2)))
  nominator <- prod(diag(tab)) - prod(c(tab[1, 2], tab[2, 1]))
  n1_row <- sum(tab[1, ])
  n2_row <- sum(tab[2, ])
  n1_col <- sum(tab[, 1])
  n2_col <- sum(tab[, 2])
  denominator <- sqrt(n1_row * n1_col * n2_row * n2_col)
  nominator / denominator
}

acf_binary <- function(x, lag_max = NULL) {
  if (is_null(lag_max)) {
    lag_max <- floor(10 * log10(length(x)))
  }
  purrr::map_dbl(seq_len(lag_max), ~ phi_coef(x, dplyr::lag(x, .x)))
}

ts_airgap_acf <- ts_airgap_dummy %>% 
  group_by(type) %>% 
  group_map(~ tibble(acf = acf_binary(.x$dummy), lag = seq_along(acf)))

ts_airgap_acf %>% 
  ggplot(aes(x = lag, y = acf)) +
  geom_col() +
  facet_wrap(~ type, ncol = 1)

ts_airgap_dummy %>%
  ggplot(aes(y = dummy)) +
  geom_acf() +
  facet_wrap(~ type, ncol = 1)

## ---- miss-types-stl
p1 <- ts_airgap_dummy %>%
  filter(type == "missing at occasional") %>%
  STL(dummy) %>%
  autoplot() +
  ggtitle("missing at occasional") +
  guides(colour = "none")
p2 <- ts_airgap_dummy %>%
  filter(type == "missing at regular") %>%
  STL(dummy) %>%
  autoplot() +
  ggtitle("missing at regular") +
  guides(colour = "none")
p3 <- ts_airgap_dummy %>%
  filter(type == "missing at runs") %>%
  STL(dummy) %>%
  autoplot() +
  ggtitle("missing at runs") +
  guides(colour = "none")
p4 <- ts_airgap_dummy %>%
  filter(type == "missing at trend") %>%
  STL(dummy) %>%
  autoplot() +
  ggtitle("missing at trend") +
  guides(colour = "none")

p1 + p2 + p3 + p4 + plot_layout(ncol = 2)

# ts_airgap_miss %>%
#   filter(type %in% c("missing at occasional", "missing at trend")) %>%
#   ACF(value) %>%
#   autoplot()

## ---- airgap
ts_airgap_gap <- count_na(ts_airgap, value)
ggplot() +
  geom_line(aes(x = index, y = value), data = ts_airgap) +
  geom_rect(
    aes(xmin = .from - 1, xmax = .to + 1), ymin = -Inf, ymax = Inf,
    data = ts_airgap_gap, alpha = 0.2
  ) +
  theme_bw()

## ---- airgap-acf
ts_airgap %>%
  mutate(dummy = ifelse(is.na(value), 1L, 0L)) %>%
  ggplot(aes(y = dummy)) +
  geom_acf()

## ---- airgap-stl
ts_airgap %>%
  mutate(dummy = ifelse(is.na(value), 1L, 0L)) %>%
  STL(dummy) %>%
  autoplot()
