library(tidyverse)

source("Scripts//data_load.R")

rural_school_spending %>%
  filter(AccountCode == 8519) %>%
  group_by(AUN) %>%
  reframe("rev_sum" = sum(Amount1)) %>%
  merge(., pa_directory, by.x = "AUN", by.y = "state_leaid") %>%
  select(AUN, rev_sum, lea_name, urban_centric_locale, enrollment)