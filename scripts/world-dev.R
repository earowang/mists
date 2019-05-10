library(tidyverse)
library(tsibble)
library(naniar)

raw_dat <- read_csv("data-raw/world-development-indicators.csv", na = "..",
  n_max = 11935)

country_code <- raw_dat %>% 
  distinct(`Country Name`, `Country Code`)
world_dev <- raw_dat %>% 
  gather(key = "Year", value = "value", `1969 [YR1969]`:last_col()) %>% 
  separate(Year, into = c("Year", "Year_String"), sep = " ") %>% 
  mutate(Year = as.integer(Year)) %>% 
  select(-contains("Code"), -Year_String) %>% 
  spread(key = `Series Name`, value = value) %>% 
  filter(Year < 2018, Year >= 1970)

vars_too_many_nas <- world_dev %>% 
  miss_var_summary() %>% 
  filter(pct_miss >= 60) %>% 
  pull(variable)

# remove variables with 80% missing values
cols_pass <- world_dev %>% 
  group_by(`Country Name`) %>% 
  summarise_all(~ sum(is.na(.)) / n()) %>% 
  mutate_all(~ . >= .8) %>% 
  summarise_all(~ sum(.) / n()) %>% 
  select_if(function(x) x < 0.8)

intersect(vars_too_many_nas, setdiff(names(world_dev), names(cols_pass))[-1])

world_dev_cols_pass <- world_dev %>% 
  select(`Country Name`, Year, names(cols_pass))

# na_sum <- function(...) {
#   lst <- list(...)
#   sum(is.na(lst)) / (length(lst) - 2L)
# }
#
# world_dev_cols_pass %>% 
#   mutate(na_cases_pct = pmap_dbl(., na_sum)) %>% 
#   slice(1:10) %>% 
#   pull(na_cases_pct)

countries_pass <- world_dev_cols_pass %>% 
  group_by(`Country Name`) %>% 
  select(-Year) %>% 
  summarise_all(na_starts_with) %>% 
  mutate(na_cols_pct = rowSums(select_if(., is.numeric) > 0) / (NCOL(.) - 1)) %>% 
  filter(na_cols_pct < 0.8) %>% 
  pull(`Country Name`)

world_dev_countries_pass <- world_dev_cols_pass %>% 
  filter(`Country Name` %in% countries_pass)

index_pass <- world_dev_countries_pass %>% 
  group_by(`Country Name`) %>% 
  select(-Year) %>% 
  summarise_all(na_starts_with) %>% 
  mutate(na_index = pmap_dbl(select_if(., is.numeric), function(...) median(c(...)))) %>% 
  pull(na_index) %>% 
  floor()

year_pass <- world_dev_countries_pass %>% 
  group_by(`Country Name`) %>% 
  summarise(Min_Year = min(Year)) %>% 
  mutate(Min_Year = Min_Year + index_pass)

world_dev_year_pass <- world_dev_countries_pass %>% 
  left_join(year_pass, by = "Country Name") %>% 
  filter(Year >= Min_Year) %>% 
  select(-Min_Year)
