library(tidyverse)
library(sf)

poverty_data %>%
  select(LEAID, young_pov_pct) %>%
  merge(.,
        pa_directory %>%
          select(leaid, urban_centric_locale) %>% 
          mutate("locale" = ifelse(grepl("Rural", urban_centric_locale), "Rural",
                                   ifelse(grepl("Town", urban_centric_locale), "Town",
                                          ifelse(grepl("Suburb", urban_centric_locale), "Suburban",
                                                 ifelse(grepl("City", urban_centric_locale), "Urban", "check"))))) %>%
          select(-urban_centric_locale),
        by.x = "LEAID", by.y = "leaid") %>%
  merge(., pa_school_districts, by.x = "LEAID", by.y = "GEOID") %>%
  filter(locale == "Rural") %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = young_pov_pct), color = "#FFFFFF") +
  scale_fill_fermenter(direction = 1, palette = "PuBuGn",
                       labels = scales::label_percent(),
                       name = "Est. % of Students in Poverty") +
  theme_void() +
  theme(text = element_text(family = "Tinos"),
        legend.position = "bottom")

pa_enrollment %>%
  filter(leaid %in% rural_school_directory$leaid) %>%
  filter(race != "Total" & race != "Unknown") %>%
  select(leaid, race, enrollment) %>%
  pivot_wider(names_from = race, values_from = enrollment) %>%
  mutate("Other Race" = `American Indian or Alaska Native` +
           `Native Hawaiian or other Pacific Islander` +
           `Two or more races`) %>%
  select(-`American Indian or Alaska Native`, 
         -`Native Hawaiian or other Pacific Islander`, 
         -`Two or more races`) %>%
  pivot_longer(cols = c(White, Black, Hispanic, Asian, `Other Race`),
               names_to = "Race", values_to = "Enrollment") %>%
  group_by(leaid) %>%
  reframe(Race = Race,
          `% Enrollment` = Enrollment / sum(Enrollment)) %>%
  merge(., pa_school_districts, by.x = "leaid", by.y = "GEOID") %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = `% Enrollment`), linewidth = 0, color = "#FFFFFF") +
  facet_wrap(vars(Race), ncol = 2) +
  theme_void() +
  scale_fill_fermenter(direction = 1, palette = "PuBuGn",
                       labels = scales::label_percent()) +
  theme(text = element_text(family = "Tinos"),
        legend.position = "bottom")