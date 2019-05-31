## ---- initial
library(tidyverse)
library(tsibble)
library(patchwork)
library(naniar)
# library(mists)
library(lubridate)
library(sugrrants)
# devtools::load_all(".")

ped <- read_rds("data-raw/ped.rds")

ped %>% 
  group_by(Sensor) %>% 
  filter(Sensor %in% c("Alfred Place", "Birrarung Marr")) %>% 
  summarise(na_runs = list_of_na_rle(Count, Date_Time)) %>% 
  mutate(na_runs = na_rle_lengths(na_runs)) %>% 
  unnest(na_runs) %>% 
  ggplot(aes(x = na_runs)) +
  geom_bar() +
  scale_x_log10() +
  facet_grid(Sensor ~ .)

x <- ped %>% 
  group_by(Sensor) %>% 
  summarise(na_runs = list_of_na_rle(Count, Date_Time)) %>% 
  pull(na_runs)

tbl_runs <- as_tibble(table(na_rle_lengths(x[[1]])), .name_repair = ~ c("runs", "n"))
tbl_runs <- 
  mutate(
    tbl_runs, 
    runs = as.integer(runs), 
    nobs = n * runs,
    xlabs = paste(runs, brackets(n), sep = "\n"),
    x = .5 * c(cumsum(nobs) + cumsum(dplyr::lag(nobs, default = 0)))
  )

tbl_runs %>% 
  ggplot(aes(x = x, y = 1, width = nobs)) +
  geom_bar(stat = "identity", colour = "white") +
  scale_x_continuous(
    labels = tbl_runs$xlabs,
    breaks = tbl_runs$x,
    minor_breaks = NULL
  ) +
  labs(x = "runs [frequency]", y = "")

ped_ts %>% 
  index_by(Time) %>% 
  summarise(n_miss = sum(is.na(Count)) / n()) %>% 
  ggplot(aes(x = Time, y = n_miss)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent)

ped_ts %>% 
  index_by(Wday = wday(Date, label = TRUE, week_start = 1)) %>% 
  summarise(n_miss = sum(is.na(Count)) / n()) %>% 
  ggplot(aes(x = Wday, y = n_miss)) +
  geom_line(group = 1) +
  geom_point() +
  scale_y_continuous(labels = scales::percent)

## ---- missing-map
# classical missingness map
ped_dummy <- ped_ts %>%
  mutate(Count_NA = if_else(is.na(Count), 1L, 0L)) %>%
  group_by_key() %>%
  mutate(
    Cum_NA = cumsum(Count_NA),
    Pct_NA = Cum_NA / n()
  ) %>%
  ungroup()

pd_abs <- ped_dummy %>%
  mutate(Sensor = as.factor(Sensor) %>% fct_reorder(Count_NA, sum)) %>%
  ggplot(aes(x = Date_Time, y = Sensor)) +
  geom_tile(aes(fill = as.factor(Count_NA))) +
  xlab("Date Time") +
  ylab("Sensor") +
  scale_fill_manual(values = c(`1` = "#af8dc3", `0` = "#7fbf7b")) +
  guides(fill = "none")

pd_rel <- ped_dummy %>%
  mutate(Sensor = as.factor(Sensor) %>% fct_reorder(Count_NA, sum)) %>%
  group_by_key() %>%
  mutate(Hour_Since = hms::as.hms(Date_Time - min(Date_Time))) %>%
  ggplot(aes(x = Hour_Since, y = Sensor)) +
  geom_tile(aes(fill = as.factor(Count_NA))) +
  xlab("Hours since") +
  ylab("") +
  scale_fill_manual(values = c(`1` = "#af8dc3", `0` = "#7fbf7b")) +
  guides(fill = "none") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

pd_scaled <- ped_dummy %>%
  mutate(Sensor = as.factor(Sensor) %>% fct_reorder(Count_NA, sum)) %>%
  group_by_key() %>%
  mutate(Scaled_Time = scale(Date_Time)) %>%
  ggplot(aes(x = Scaled_Time, y = Sensor)) +
  geom_tile(aes(fill = as.factor(Count_NA))) +
  xlab("Scaled Time") +
  ylab("") +
  scale_fill_manual(values = c(`1` = "#af8dc3", `0` = "#7fbf7b")) +
  guides(fill = "none") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

pd_abs + pd_rel + pd_scaled

## ---- missing-dots
# dot & range plots
# absolute time
ped_range <- ped %>%
  group_by(Sensor) %>%
  summarise(.from = min(Date_Time), .to = max(Date_Time))
p_abs <- ped_ts %>%
  count_na(Count) %>%
  mutate(Sensor = as.factor(Sensor) %>% fct_reorder(.n, sum)) %>%
  ggplot(aes(x = Sensor)) +
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from), shape = 4) +
  geom_point(aes(y = .to), shape = 4) +
  geom_point(aes(y = .from), colour = "grey70", data = ped_range) +
  # geom_point(aes(y = .to), colour = "grey70", data = ped_range) +
  coord_flip() +
  xlab("Sensor") +
  ylab("Date Time")

# relative time since the beginning of each sensor
# each sensor starts at the same time point
ped_range_relative <- ped_range %>%
  group_by(Sensor) %>%
  mutate(.to = hms::as.hms(.to - .from))
p_rel <- ped_ts %>%
  group_by_key() %>%
  mutate(Hour_Since = hms::as.hms(Date_Time - min(Date_Time))) %>%
  update_tsibble(index = Hour_Since) %>%
  count_na(Count) %>%
  mutate(Sensor = as.factor(Sensor) %>% fct_reorder(.n, sum)) %>%
  ggplot(aes(x = Sensor)) +
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from), shape = 4) +
  geom_point(aes(y = .to), shape = 4) +
  geom_point(aes(y = .to), colour = "grey70", data = ped_range_relative) +
  coord_flip() +
  xlab("") +
  ylab("Hours since") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

# centered scale time for each sensor
# each sensor has its own scale
p_scaled <- ped_ts %>%
  group_by_key() %>%
  mutate(Scaled_Time = scale(Date_Time)) %>%
  update_tsibble(index = Scaled_Time) %>%
  count_na(Count) %>%
  mutate(Sensor = as.factor(Sensor) %>% fct_reorder(.n, sum)) %>%
  ggplot(aes(x = Sensor)) +
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from), shape = 4) +
  geom_point(aes(y = .to), shape = 4) +
  coord_flip() +
  xlab("") +
  ylab("Scaled Time") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

p_abs + p_rel + p_scaled

## ---- missing-occurrence
# the number of consecutive missingess using occurrence dot plots
ped_ts %>%
  count_na(Count) %>%
  # mutate(binning = cut(.n, seq(0, 15000, 5000))) %>%
  mutate(Sensor = as.factor(Sensor) %>% fct_reorder(.n, sum)) %>%
  # filter(.n <= 5000) %>%
  ggplot(aes(x = .n)) +
  geom_dotplot(binwidth = 50) +
  # facet_grid(Sensor ~ binning, scales = "free_x") +
  facet_wrap(~ Sensor, ncol = 1, strip.position = "right") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

## ---- flinders-corr
ped_ts %>%
  filter(
    Sensor %in% c("Flinders St-Elizabeth St (East)", "Flinders Street Station Underpass")
  ) %>%
  select(Sensor, Date_Time, Count) %>%
  group_by_key() %>%
  spread(Sensor, Count) %>%
  ggplot(aes(x = `Flinders Street Station Underpass`, y = `Flinders St-Elizabeth St (East)`)) +
  geom_miss_point(alpha = 0.6, size = 0.2) +
  coord_fixed(ratio = 1)

## ---- flinders-corr-weekend
ped_ts %>%
  filter(
    Sensor %in% c("Flinders St-Elizabeth St (East)", "Flinders Street Station Underpass")
  ) %>%
  select(Sensor, Date_Time, Count) %>%
  group_by_key() %>%
  spread(Sensor, Count) %>%
  mutate(
    Wday = wday(Date_Time, label = TRUE, week_start = 1),
    Weekend = ifelse(Wday %in% c("Sat", "Sun"), TRUE, FALSE)
  ) %>%
  ggplot(aes(x = `Flinders Street Station Underpass`, y = `Flinders St-Elizabeth St (East)`)) +
  geom_miss_point(alpha = 0.6, size = 0.2) +
  facet_grid(~ Weekend) +
  coord_fixed(ratio = 1)

## ---- flinders-corr-hours
ped_ts %>%
  filter(
    Sensor %in% c("Flinders St-Elizabeth St (East)", "Flinders Street Station Underpass")
  ) %>%
  select(Sensor, Date_Time, Count) %>%
  group_by_key() %>%
  spread(Sensor, Count) %>%
  mutate(Time = hour(Date_Time), Is8 = ifelse(Time %in% 6:8, TRUE, FALSE)) %>%
  ggplot(aes(x = `Flinders Street Station Underpass`, y = `Flinders St-Elizabeth St (East)`, colour = Is8)) +
  geom_point(alpha = 0.6, size = 0.2) +
  coord_fixed(ratio = 1)

## ---- flinders-corr-hours-facet
ped_ts %>%
  filter(
    Sensor %in% c("Flinders St-Elizabeth St (East)", "Flinders Street Station Underpass")
  ) %>%
  select(Sensor, Date_Time, Count) %>%
  group_by_key() %>%
  spread(Sensor, Count) %>%
  mutate(Time = hour(Date_Time)) %>%
  ggplot(aes(x = `Flinders Street Station Underpass`, y = `Flinders St-Elizabeth St (East)`)) +
  geom_miss_point(alpha = 0.6, size = 0.2) +
  facet_wrap(~ Time, ncol = 4) +
  coord_fixed(ratio = 1)

## ---- lagplot
sensors_fct <- as_tibble(ped_ts) %>%
  group_by(Sensor) %>%
  summarise(.n = sum(is.na(Count))) %>%
  mutate(
    Sensor = as.factor(Sensor) %>% fct_reorder(.n, sum, .desc = TRUE)
      %>% fct_lump(12, ties.method = "first")
  ) %>%
  pull(Sensor)

ped_ts %>%
  filter(Sensor %in% sensors_fct) %>%
  mutate(Lag_Count = lag(Count)) %>%
  ggplot(aes(x = Count, y = Lag_Count)) +
  geom_miss_point(alpha = 0.6, size = 0.2) +
  facet_wrap(~ Sensor, ncol = 4, scales = "free") +
  theme(legend.position = "bottom")

## ---- acfplot
ped_dummy %>%
  filter(Sensor %in% sensors_fct) %>%
  ggplot(aes(y = Count)) +
  geom_acf() +
  facet_wrap(~ Sensor, ncol = 1, strip.position = "right")

## ---- misc
ped_dummy %>%
  # filter(Sensor == "Alfred Place") %>%
  ggplot(aes(x = Date_Time, y = Cum_NA)) +
  geom_line() +
  facet_wrap(~ Sensor, ncol = 1, strip.position = "right")

ped %>%
  group_by(Sensor) %>%
  summarise(
    n_na = sum(is.na(Count)),
    pct_na = n_na / n()
  ) %>%
  arrange(desc(pct_na))

ped %>%
  filter(Sensor == "Bourke Street Mall (South)") %>%
  ggplot(aes(x = Date_Time, y = Count)) +
  geom_line()

ped %>% 
  # filter(Sensor == "Alfred Place") %>% 
  group_by(Sensor) %>% 
  summarise(na_rle = list(na_rle(Count))) %>% 
  unnest(na_rle) %>% 
  filter(rle != "0") %>% 
  ggplot(aes(x = Sensor, y = n, fill = as.factor(rle))) +
  geom_col(position = "fill")

ped %>% 
  group_by(Sensor) %>% 
  summarise(na_rle = list(na_rle(Count))) %>% 
  unnest(na_rle) %>% 
  filter(rle != "0") %>% 
  count(Sensor)
