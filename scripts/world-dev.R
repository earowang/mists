library(tidyverse)
library(tsibble)
library(rlang)

raw_dat <- read_csv("data-raw/world-development-indicators.csv", na = "..",
  n_max = 11935)

country_code_df <- raw_dat %>% 
  distinct(`Country Name`, `Country Code`) %>% 
  rename_all(janitor::make_clean_names) %>% 
  left_join(
    countrycode::codelist %>% select(iso3c, region, continent),
    by = c("country_code" = "iso3c")
  ) %>% 
  arrange(continent, region) %>% 
  mutate(country_code = fct_infreq(country_code))
series_code_df <- raw_dat %>% 
  distinct(`Series Name`, `Series Code`) %>% 
  rename_all(janitor::make_clean_names)
world_dev <- raw_dat %>% 
  gather(key = "Year", value = "value", `1969 [YR1969]`:last_col()) %>% 
  rename_all(janitor::make_clean_names) %>% 
  separate(year, into = c("year", "year_string"), sep = " ") %>% 
  mutate(year = as.integer(year)) %>% 
  select(-contains("name"), -year_string) %>% 
  spread(key = series_code, value = value) %>% 
  mutate(
    country_code = factor(country_code, levels = country_code_df$country_code)
  )

naniar:::miss_prop_summary.default

naniar::miss_prop_summary(world_dev)

prop_overall_na(world_dev)

world_dev_ts <- world_dev %>% 
  as_tsibble(key = country_code, index = year)

wdi_cols_pass <- polish_cols_measures(world_dev_ts, cutoff = 0.8)
polish_metrics(world_dev_ts, wdi_cols_pass)

wdi_key_pass <- polish_rows_key(world_dev_ts, cutoff = 0.8)
polish_metrics(world_dev_ts, wdi_key_pass)

wdi_key_pass <- polish_rows_index(wdi_cols_pass)
polish_metrics(world_dev_ts, wdi_key_pass)

polish_rows_index(world_dev_ts)

# world_dev %>% 
#   group_nest(year, .key = "pct_overall_na") %>% 
#   mutate(pct_overall_na = map_dbl(pct_overall_na, prop_overall_miss)) %>% 
#   filter(pct_overall_na < 0.8)

keyed_data <- group_by(world_dev, country_code)
idx_len <- map_int(group_rows(keyed_data), length)
keyed_nobs <- idx_len * NCOL(world_dev)

index_pass <- world_dev %>% 
  group_by(country_code) %>% 
  summarise_all(na_starts_with) %>% 
  group_nest(country_code, .key = "..n_na") %>% 
  mutate(..n_na = floor(map_dbl(..n_na, sum) / keyed_nobs * idx_len))

world_dev %>% 
  left_join(index_pass, by = "country_code") %>% 
  group_by(country_code) %>% 
  filter(year >= min(year) + ..n_na) %>% 
  select(-..n_na)

world_dev %>% 
  group_by(country_code) %>% 
  summarise_all(na_ends_with) %>% 
  group_nest(country_code, .key = "pct_overall_na") %>% 
  mutate(pct_overall_na = map_dbl(pct_overall_na, sum) / keyed_nobs)

world_dev_year_pass %>% 
  group_by(country_code) %>% 
  summarise_at(vars(3:last_col()), ~ sum(is.na(.)) / n()) %>% 
  gather("variable", "value", AG.SRF.TOTL.K2:last_col()) %>% 
  ggplot(aes(x = country_code, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme(axis.text.x = element_text(angle = 90))

world_dev_year_pass %>% 
  group_by(country_code) %>% 
  summarise_at(vars(3:last_col()), ~ sum(is.na(.)) / n()) %>% 
  gather("variable", "value", AG.SRF.TOTL.K2:last_col()) %>% 
  ggplot(aes(x = country_code, y = variable, size = value)) +
  geom_point(shape = 18)

world_dev_year_pass %>% 
  gather("variable", "value", AG.LND.FRST.K2:last_col()) %>% 
  mutate(na_value = ifelse(is.na(value), 1L, 0L)) %>% 
  group_by(country_code, variable) %>% 
  summarise(lag1 = acf_binary(na_value, 1)) %>% 
  ggplot(aes(x = country_code, y = variable, fill = lag1)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme(axis.text.x = element_text(angle = 90))
