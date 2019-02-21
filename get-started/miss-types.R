library(tidyverse)
library(lubridate)
library(tsibble)
library(imputeTS)
library(mists)
library(rlang)
library(patchwork)
set.seed(2019)

tbl <- tibble(xmin = 1:2, xmax = 2:3, group = 1:2)
ggplot(tbl) +
  geom_rect(aes(xmin = xmin - 1, xmax = xmax + 1), ymin = -Inf, ymax = Inf) +
  facet_wrap(~ group, ncol = 1)

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
  rnd_len <- sample(seq_len(NROW(.data) * 0.1), size = length(rnd_init))
  rnd_run <- do.call("c", purrr::map2(rnd_init, rnd_len, ~ .x + seq_len(.y)))
  mutate(
    .data,
    value = ifelse(idx %in% rnd_run, NA, value)
  )
}

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
    data = count_na(ts_airgap_miss, value), alpha = 0.2
  ) +
  facet_wrap(~ type, ncol = 1, strip.position = "right")

ts_airgap_gap <- count_na(ts_airgap, value)

ggplot() +
  geom_line(aes(x = index, y = value), data = ts_airgap) +
  geom_rect(
    aes(xmin = .from - 1, xmax = .to + 1, ymin = -Inf, ymax = Inf),
    data = ts_airgap_gap, alpha = 0.2
  )
