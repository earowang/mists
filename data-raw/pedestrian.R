library(tidyverse)

ped <- rwalkr::melb_walk_fast(year = 2016:2018)

ped_sensor <- rwalkr::lookup_sensor()$run
ped_sensor_date <- rwalkr::pull_sensor() %>% 
  select(Sensor, Year_Installed)

first_non_na <- function(x, y) {
  y[which.min(is.na(x))]
}

ped_clean <- ped %>% 
  filter(Sensor %in% ped_sensor) %>% # remove sensors that are not documented
  mutate(Year = as.Date(Date)) %>% 
  left_join(ped_sensor_date, by = "Sensor") %>% 
  filter(Year_Installed <= Year) %>% # rm obs b/f the year installed
  group_by(Sensor) %>% 
  filter(first_non_na(Count, Date) <= Date) %>% 
  ungroup()

readr::write_rds(ped_clean, "data-raw/ped.rds", compress = "xz")
