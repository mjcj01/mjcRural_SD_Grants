library(tidyverse)
library(educationdata)

revenue_22_23 <- read_csv("Data//revenue_22_23.csv")

revenue_overview_22_23 <- read_csv("Data//revenue_overview_22_23.csv")

colnames(revenue_overview_22_23) <- c("AUN",
                                      "school_district",
                                      "county",
                                      "tot_rev",
                                      "loc_tax_rev",
                                      "loc_oth_rev",
                                      "loc_tot_rev",
                                      "sta_tot_rev",
                                      "fed_tot_rev",
                                      "oth_tot_rev")

adm_22_23 <- read_csv("Data//adm_22_23.csv")

colnames(adm_22_23) <- c("AUN", "school_district", "county", "adm", "wadm")

adm_22_23 <- adm_22_23 %>%
  select("AUN", "adm", "wadm")

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

rural_school_directory <- pa_directory %>%
  filter(grepl("Rural", urban_centric_locale))

rural_school_enrollment <- pa_enrollment %>%
  filter(leaid %in% rural_school_directory$leaid)

rural_school_spending <- revenue_22_23 %>%
  filter(AUN %in% rural_school_directory$state_leaid)

nces_locale_classifications <- tibble("Rural Classification" = c("Fringe", "Distant", "Remote"),
                                      "Criteria" = c("5 miles or closer to urban area with 50,000 or more people, and is 2.5 miles or closer to urban area with less than 50,000 people.",
                                                     "5 - 25 miles from an urban area with 50,000 or more people, and is 2.5 - 10 miles from an urban area with less than 50,000 people.",
                                                     "25 miles from an urban area with 50,000 or more people, and more than 10 miles from an urban area with less than 50,000 people."))