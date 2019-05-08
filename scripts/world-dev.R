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
  filter(pct_miss > 50) %>% 
  pull(variable)

world_dev_sub <- world_dev %>% 
  select(-vars_too_many_nas)

world_dev_ts <- world_dev_sub %>% 
  as_tsibble(key = `Country Name`, index = Year)

world_dev_ts %>% 
  filter(`Country Name` == "New Zealand") %>% 
  select(contains("Urban Population")) %>% 
  ggplot(aes(x = Year, y = `Urban population growth (annual %)`)) +
  geom_line() +
  geom_point()

world_dev_ts %>% 
  group_by(`Country Name`) %>% 
  mists::count_na(`Urban population growth (annual %)`)

x <- world_dev_sub %>% 
  group_by(`Country Name`) %>% 
  summarise(na_rle = list(na_rle(`Urban population growth (annual %)`))) %>% 
  unnest(na_rle)
x %>% 
  filter(rle != 0) %>% 
  ggplot(aes(x = `Country Name`, y = n, fill = as.factor(rle))) +
  geom_col(position = "fill")

sebria <- world_dev_sub %>% 
  filter(`Country Name` == "Serbia") %>% 
  select(`Urban population growth (annual %)`) %>% 
  pull(1)

world_dev_sub %>% 
  group_by(`Country Name`) %>% 
  summarise_all(na_starts_with)
world_dev_sub %>% 
  group_by(`Country Name`) %>% 
  summarise_all(na_ends_with)
