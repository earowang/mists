library(tidyverse)

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

na_rle_wdi <- world_dev %>% 
  group_by(country_code) %>% 
  summarise_at(vars(3:last_col()), ~ list_of_na_rle(., year))

na_rle_wdi$BX.KLT.DINV.CD.WD[1:2]
class(na_rle_wdi$BX.KLT.DINV.CD.WD)

prop_overall_na(world_dev)

world_dev_ts <- world_dev %>% 
  as_tsibble(key = country_code, index = year)

wdi_cols_pass <- polish_cols_measures(world_dev_ts, cutoff = 0.8)
polish_metrics(world_dev_ts, wdi_cols_pass)

wdi_key_pass <- polish_rows_key(world_dev_ts, cutoff = 0.8)
polish_metrics(world_dev_ts, wdi_key_pass)

wdi_key_pass <- polish_rows_index(wdi_cols_pass)
polish_metrics(world_dev_ts, wdi_key_pass)

polish_rows_index(world_dev_ts, na_fun = na_ends_with)

na_rle(world_dev_ts$AG.LND.FRST.K2)
