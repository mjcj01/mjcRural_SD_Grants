library(tidyverse)
library(haven)

nces_locale_classifications <- tibble("Rural Classification" = c("Fringe", "Distant", "Remote"),
                                      "Criteria" = c("5 miles or closer to urban area with 50,000 or more people, and is 2.5 miles or closer to urban area with less than 50,000 people.",
                                                     "5 - 25 miles from an urban area with 50,000 or more people, and is 2.5 - 10 miles from an urban area with less than 50,000 people.",
                                                     "25 miles from an urban area with 50,000 or more people, and more than 10 miles from an urban area with less than 50,000 people."))


nces_rev <- read_sas("Data//sdf22_1a.sas7bdat") %>%
  filter(STABBR == "PA" & grepl(" SD", NAME))

# pa_directory <- get_education_data(level = 'school-districts',
#                                    source = 'ccd',
#                                    topic = 'directory',
#                                    filters = list(year = 2022,
#                                                   fips = 42),
#                                    add_labels = TRUE) %>%
#   filter(agency_type == "Regular local school district") %>%
#   mutate(state_leaid = gsub("PA-", "", state_leaid))
# 
# write_rds(pa_directory, "Data//pa_directory_2022.Rds")

pa_directory <- read_rds("Data//pa_directory_2022.Rds")

# pa_enrollment <- get_education_data(level = 'school-districts',
#                          source = 'ccd',
#                          topic = 'enrollment',
#                          subtopic = list('race'),
#                          filters = list(year = 2022,
#                                         fips = 42,
#                                         grade = 99),
#                          add_labels = TRUE) %>%
#   filter(leaid %in% pa_directory$leaid)
# 
# write_rds(pa_enrollment, "Data//pa_enrollment_2022.Rds")

pa_enrollment <- read_rds("Data//pa_enrollment_2022.Rds")

nces_rev %>%
  select(LEAID, NAME, TOTALREV, TFEDREV, TSTREV,
         C14, C15, C19, C22, C26, C27, B11, C20, C25, C36, B10, B12, B14, B13) %>%
  merge(.,
        pa_directory %>%
          select(leaid, state_leaid, urban_centric_locale) %>%
          mutate("locale" = ifelse(grepl("Rural", urban_centric_locale), "Rural",
                            ifelse(grepl("Town", urban_centric_locale), "Town",
                            ifelse(grepl("Suburb", urban_centric_locale), "Suburban",
                            ifelse(grepl("City", urban_centric_locale), "Urban", "check"))))) %>%
          select(-urban_centric_locale),
        by.x = "LEAID", by.y = "leaid") %>%
  pivot_longer(cols = c(C14, C15, C19, C22, C26, C27, B11, C20, C25, C36, B10, B12, B14, B13)) %>%
  merge(.,
        pa_enrollment %>%
          filter(race == "Total") %>%
          select(leaid, enrollment),
        by.x = "LEAID", by.y = "leaid") %>%
  mutate("rev_per_enr" = value / enrollment) %>%
  filter(name == "C25") %>%
  ggplot(aes(x = enrollment, y = value)) +
  geom_point() +
  facet_wrap(vars(name))
  