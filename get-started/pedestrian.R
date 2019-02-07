library(tidyverse)
library(tsibble)

ped <- read_rds("data-raw/ped.rds")
ped_ts <- as_tsibble(ped, key = id(Sensor), index = Date_Time, validate = FALSE)

ped_range <- ped %>% 
  group_by(Sensor) %>% 
  summarise(.from = min(Date_Time), .to = max(Date_Time))

ped_ts %>% 
  count_na(Count) %>% 
  mutate(Sensor = as.factor(Sensor) %>% fct_reorder(.n, sum)) %>% 
  ggplot(aes(x = Sensor)) +
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from), shape = 4) +
  geom_point(aes(y = .to), shape = 4) +
  geom_point(aes(y = .from), colour = "red", data = ped_range) +
  geom_point(aes(y = .to), colour = "red", data = ped_range) +
  coord_flip() +
  xlab("Sensor") +
  ylab("Time gaps") +
  theme(legend.position = "none")

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
