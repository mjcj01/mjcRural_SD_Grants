library(tidyverse)
library(sf)
library(haven)
library(tigris)

fed_rev_shiny <- read_sas("Data//sdf22_1a.sas7bdat") %>%
  filter(STABBR == "PA" & grepl(" SD", NAME)) %>%
  select(LEAID, NAME, C14, C15, C19, C22, C23, C26, C27, B11, C20, C25, C36, B10, B12, B14)

colnames(fed_rev_shiny) <- c("LEAID", "district_name", "title_i", "idea", "cte", "eff_inst", "ac_enrch",
                             "21_cent", "rlis", "biling", "other", "child_nutr", "nonsp",
                             "impact", "ind_edu", "srsa", "other2")

fed_rev_shiny <- fed_rev_shiny %>%
  select(LEAID, district_name, title_i, idea, cte, child_nutr)

pa_dir <- read_rds("Data//pa_directory_2022.Rds") %>%
  select(leaid, urban_centric_locale) %>%
  mutate(locale = ifelse(grepl("Rural", urban_centric_locale), "Rural",
                  ifelse(grepl("Town", urban_centric_locale), "Town",
                  ifelse(grepl("Suburb", urban_centric_locale), "Suburban",
                  ifelse(grepl("City", urban_centric_locale), "Urban", "Check"))))) %>%
  select(-urban_centric_locale)
pa_enr <- read_rds("Data//pa_enrollment_2022.Rds") %>%
  select(leaid, race, enrollment) %>%
  pivot_wider(names_from = race,
              values_from = enrollment) %>%
  select(leaid, Total, White, Black, Hispanic, Asian) %>%
  pivot_longer(cols = c("White", "Black", "Hispanic", "Asian"),
               names_to = "race",
               values_to = "enrollment") %>%
  mutate("pct_enr" = enrollment / Total) %>%
  merge(., pa_dir, by = "leaid")

school_districts <- school_districts(state = "PA", cb = TRUE) %>%
  select(GEOID, geometry)

fed_rev_shiny <- fed_rev_shiny %>%
  merge(., pa_enr, by.x = "LEAID", by.y = "leaid")

fed_rev_shiny %>%
  filter(grepl("Downingtown", district_name)) %>%
  select(race, enrollment, Total) %>%
  reframe("pct_enr" = enrollment/Total,
          "race" = race,
          "Total" = Total,
          "Other" = Total - sum(enrollment)) %>%
  pivot_wider(names_from = "race", values_from = "pct_enr") %>%
  mutate(Other = Other / Total) %>%
  mutate(year = 1234) %>%
  pivot_longer(cols = c("Black", "Asian", "Hispanic", "White", "Other")) %>%
  select(year, name, value) %>%
  ggplot(aes(x = year, y = value, fill = name)) +
  geom_col() +
  coord_flip() +
  labs(y = "% Enrollment") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(title = "Race")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank())

st_write(school_districts, "Shiny Data//school_districts.shp", append = FALSE)
write_rds(fed_rev_shiny, "Shiny Data//fed_rev_shiny.rds")
