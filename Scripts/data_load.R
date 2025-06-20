library(tidyverse)
library(educationdata)
library(haven)
library(sf)
library(tigris)
library(tidycensus)

nces_locale_classifications <- tibble("Rural Classification" = c("Fringe", "Distant", "Remote"),
                                      "Criteria" = c("5 miles or closer to urban area with 50,000 or more people, and is 2.5 miles or closer to urban area with less than 50,000 people.",
                                                     "5 - 25 miles from an urban area with 50,000 or more people, and is 2.5 - 10 miles from an urban area with less than 50,000 people.",
                                                     "25 miles from an urban area with 50,000 or more people, and more than 10 miles from an urban area with less than 50,000 people."))

# pa_school_districts <- school_districts(state = 42) %>%
#   st_transform(crs = 26917) %>%
#   select(GEOID, NAME, geometry)
# 
# st_write(pa_school_districts, "Data//School Districts//pa_school_districts.shp")

pa_school_districts <- st_read("Data//School Districts//pa_school_districts.shp")
pa_school_districts_centroid <- st_centroid(pa_school_districts)

nces_rev <- read_sas("Data//sdf22_1a.sas7bdat") %>%
  filter(STABBR == "PA" & grepl(" SD", NAME))

# pa_directory <- get_education_data(level = 'school-districts',
#                                    source = 'ccd',
#                                    topic = 'directory',
#                                    filters = list(year = 2021,
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
#                          filters = list(year = 2021,
#                                         fips = 42,
#                                         grade = 99),
#                          add_labels = TRUE) %>%
#   filter(leaid %in% pa_directory$leaid)
# 
# write_rds(pa_enrollment, "Data//pa_enrollment_2022.Rds")

pa_enrollment <- read_rds("Data//pa_enrollment_2022.Rds") %>%
  mutate(ifelse(enrollment < 0, 0, enrollment))

fed_rev <- nces_rev %>%
  select(LEAID, NAME, TOTALREV, TFEDREV, TSTREV,
         C22, C15, B11, C19, C25) %>%
  merge(.,
        pa_directory %>%
          select(leaid, state_leaid, urban_centric_locale) %>%
          mutate("locale" = ifelse(grepl("Rural", urban_centric_locale), "Rural",
                            ifelse(grepl("Town", urban_centric_locale), "Town",
                            ifelse(grepl("Suburb", urban_centric_locale), "Suburban",
                            ifelse(grepl("City", urban_centric_locale), "Urban", "check"))))) %>%
          select(-urban_centric_locale),
        by.x = "LEAID", by.y = "leaid") %>%
  pivot_longer(cols = c(C22, C15, C19, C25)) %>%
  merge(.,
        pa_enrollment %>%
          filter(race == "Total") %>%
          select(leaid, enrollment),
        by.x = "LEAID", by.y = "leaid") %>%
  mutate(value = ifelse(value < 0, 0, value)) %>%
  mutate("rev_per_enr" = value / enrollment) %>%
  select(-value, -enrollment) %>%
  pivot_wider(names_from = "name",
              values_from = "rev_per_enr") %>%
  rename(`Teacher Funding` = "C22",
         `Special Education` = "C15",
         `Career and Technical Education` = "C19",
         `School Meal Programs` = "C25") %>%
  pivot_longer(cols = c("Teacher Funding", "Special Education", "Career and Technical Education", "School Meal Programs"),
               values_to = "fed_rev") %>%
  select(LEAID, locale, name, fed_rev)

sta_rev <- nces_rev %>%
  select(LEAID, NAME, TOTALREV, TFEDREV, TSTREV,
         C04, C05, C07, C09, C10) %>%
  merge(.,
        pa_directory %>%
          select(leaid, state_leaid, urban_centric_locale) %>%
          mutate("locale" = ifelse(grepl("Rural", urban_centric_locale), "Rural",
                            ifelse(grepl("Town", urban_centric_locale), "Town",
                            ifelse(grepl("Suburb", urban_centric_locale), "Suburban",
                            ifelse(grepl("City", urban_centric_locale), "Urban", "check"))))) %>%
          select(-urban_centric_locale),
        by.x = "LEAID", by.y = "leaid") %>%
  pivot_longer(cols = c(C04, C05, C09, C10)) %>%
  merge(.,
        pa_enrollment %>%
          filter(race == "Total") %>%
          select(leaid, enrollment),
        by.x = "LEAID", by.y = "leaid") %>%
  mutate(value = ifelse(value < 0, 0, value)) %>%
  mutate("rev_per_enr" = value / enrollment) %>%
  select(-value, -enrollment) %>%
  pivot_wider(names_from = "name",
              values_from = "rev_per_enr") %>%
  rename(`Teacher Funding` = "C04",
         `Special Education` = "C05",
         `Career and Technical Education` = "C09",
         `School Meal Programs` = "C10") %>%
  pivot_longer(cols = c("Teacher Funding", "Special Education", "Career and Technical Education", "School Meal Programs"),
               values_to = "sta_rev") %>%
  select(LEAID, locale, name, sta_rev)

fed_sta_comp <- merge(fed_rev, sta_rev, by = c("LEAID", "locale", "name")) %>%
  group_by(name) %>%
  mutate("fed_pct" = percent_rank(fed_rev),
         "sta_pct" = percent_rank(sta_rev)) %>%
  merge(.,
        pa_enrollment %>%
          filter(race == "Total") %>%
          select(leaid, enrollment),
        by.x = "LEAID", by.y = "leaid")

poverty_data <- read_csv("Data//poverty_data_2021_22.csv") %>%
  filter(state == "PA") %>%
  mutate(LEAID = paste(42, LEAID, sep = "")) %>%
  mutate("young_pov_pct" = young_pov_pop / young_pop,
         "tot_pov_pct" = young_pov_pop / tot_pop)

rural_school_directory <- pa_directory %>%
  filter(grepl("Rural", urban_centric_locale))

# variables <- c("Hispanic" = "P2_002N", "White" = "P2_005N", "Black" = "P2_006N", "Asian" = "P2_008N")
# 
# census_pop <- get_decennial(geography = "school district (unified)",
#               variables = variables,
#               summary_var = "P2_001N",
#               state = "PA",
#               year = 2020) %>%
#   mutate("pop_pct" = value / summary_value)
# 
# write_rds(census_pop, "Data//census_pop.rds")

census_pop <- read_rds("Data//census_pop.rds")

crdc_spled_enr <- read_csv("Data//Enrollment.csv") %>%
  filter(LEA_STATE == "PA") %>%
  select(LEAID, SCHID, SCH_ENR_IDEA_M, SCH_ENR_IDEA_F, SCH_IDEAENR_HI_M, SCH_IDEAENR_HI_F,
         SCH_IDEAENR_AS_M, SCH_IDEAENR_AS_F,
         SCH_IDEAENR_WH_M, SCH_IDEAENR_WH_F, SCH_IDEAENR_BL_M, SCH_IDEAENR_BL_F,
         SCH_ENR_504_M, SCH_ENR_504_F, SCH_504ENR_HI_M, SCH_504ENR_HI_F,
         SCH_504ENR_AS_M, SCH_504ENR_AS_F,
         SCH_504ENR_WH_M, SCH_504ENR_WH_F, SCH_504ENR_BL_M, SCH_504ENR_BL_F)

crdc_spled_enr[crdc_spled_enr < 0] <- 0

crdc_spled_enr <- crdc_spled_enr %>%
  group_by(LEAID) %>%
  reframe(TOT_IDEA = SCH_ENR_IDEA_M + SCH_ENR_IDEA_F,
          HISP_IDEA = SCH_IDEAENR_HI_M + SCH_IDEAENR_HI_F,
          ASIAN_IDEA = SCH_IDEAENR_AS_M + SCH_IDEAENR_AS_F,
          WHITE_IDEA = SCH_IDEAENR_WH_M + SCH_IDEAENR_WH_F,
          BLACK_IDEA = SCH_IDEAENR_BL_M + SCH_IDEAENR_BL_F,
          TOT_504 = SCH_ENR_504_M + SCH_ENR_504_F,
          HISP_504 = SCH_504ENR_HI_M + SCH_504ENR_HI_F,
          ASIAN_504 = SCH_504ENR_AS_M + SCH_504ENR_AS_F,
          WHITE_504 = SCH_504ENR_WH_M + SCH_504ENR_WH_F,
          BLACK_504 = SCH_504ENR_BL_M + SCH_504ENR_BL_F,
          TOT_SPLED = TOT_IDEA + TOT_504,
          HISP_SPLED = HISP_IDEA + HISP_504,
          ASIAN_SPLED = ASIAN_IDEA + ASIAN_504,
          WHITE_SPLED = WHITE_IDEA + WHITE_504,
          BLACK_SPLED = BLACK_IDEA + BLACK_504) %>%
  select(LEAID, contains("IDEA")) %>%
  rename(Black = "BLACK_IDEA",
         Hispanic = "HISP_IDEA",
         White = "WHITE_IDEA",
         Asian = "ASIAN_IDEA",
         Total = "TOT_IDEA") %>%
  group_by(LEAID) %>%
  reframe(Total = sum(Total),
          Hispanic = sum(Hispanic),
          Asian = sum(Asian),
          White = sum(White),
          Black = sum(Black)) %>%
  filter(LEAID %in% pa_enrollment$leaid)

title_i_nces <- nces_rev %>%
  select(LEAID, TOTALREV, TFEDREV, V33, C14) %>%
  mutate("title1_per_enr" = C14 / V33) %>%
  merge(., poverty_data, by = "LEAID")

fed_rural_prog_rev <- nces_rev %>%
  select(LEAID)