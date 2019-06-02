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
# series_code_df <- raw_dat %>% 
#   distinct(`Series Name`, `Series Code`) %>% 
#   rename_all(janitor::make_clean_names) %>% 
#   mutate(series_code = janitor::make_clean_names)
wdi <- raw_dat %>% 
  gather(key = "Year", value = "value", `1969 [YR1969]`:last_col()) %>% 
  rename_all(janitor::make_clean_names) %>% 
  separate(year, into = c("year", "year_string"), sep = " ") %>% 
  mutate(year = as.integer(year)) %>% 
  select(-contains("name"), -year_string) %>% 
  spread(key = series_code, value = value) %>% 
  mutate(
    country_code = factor(country_code, levels = country_code_df$country_code)
  ) %>% 
  rename_all(janitor::make_clean_names)

usethis::use_data(wdi, overwrite = TRUE, compress = "xz")
