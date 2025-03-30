library(tidyverse)
library(educationdata)

revenue_22_23 <- read_csv("Data//revenue_22_23.csv") 

fed_revenue_22_23 <- revenue_22_23 %>%
  mutate("Funding Classification" = ifelse(AccountCode %in% c(8100, 8110, 8190, 8200), "Unrestricted Federal Grants",
                                    ifelse(AccountCode %in% c(8310, 8320), "Generic Unrestricted Federal Grants",
                                    ifelse(AccountCode %in% c(8330), "Title V, Part B, Subpart 1 Funding",
                                    ifelse(AccountCode %in% c(8512,  8513, 6832, 6833, 6834), "Special Education Funding",
                                    ifelse(AccountCode %in% c(6837, 6838, 8731, 8732, 8733, 8735, 
                                                              8741, 8742, 8743, 8744, 8745, 8746, 
                                                              8747, 8748, 8749, 8751, 8752, 8753, 
                                                              8754, 8755, 8756), "Federal Stimulus Funding",
                                    ifelse(AccountCode %in% c(8530, 8531, 8532, 8533, 8534, 8540), "Nutrition Funding and Training",
                                    ifelse(AccountCode %in% c(8560), "Block Grants",
                                    ifelse(AccountCode %in% c(8580, 8640), "Child Care and Development",
                                    ifelse(AccountCode %in% c(6836), "Race to the Top",
                                    ifelse(AccountCode %in% c(8810, 8820, 8830), "Medical Assistance",
                                    ifelse(AccountCode %in% c(8610), "Homeless Assistance",
                                    ifelse(AccountCode %in% c(8620), "Adult Education",
                                    ifelse(AccountCode %in% c(8660), "Workforce Education",
                                    ifelse(AccountCode %in% c(8521, 8522), "Vocational Education",
                                    ifelse(AccountCode %in% c(6890, 8390, 8690), "Other Federal Revenue Sources",
                                    ifelse(AccountCode %in% c(8519), "Federal Funding From ESEA",
                                    ifelse(AccountCode %in% c(8516), "Language Instruction for English Learners and Immigrant Students",
                                    ifelse(AccountCode %in% c(8517), "School Improvement Funding",
                                    ifelse(AccountCode %in% c(8514), "Low Income Assistance",
                                    ifelse(AccountCode %in% c(8515), "Helping At-Risk Students",
                                    ifelse(AccountCode %in% c(8511), "Other ESEA or IDEA Funding", "check")))))))))))))))))))))) %>%
  filter(`Funding Classification` != "check") %>%
  mutate(Amount2 = ifelse(is.na(Amount2), 0, Amount2),
         Amount3 = ifelse(is.na(Amount3), 0, Amount3),
         TotalAmount = Amount1 + Amount2 + Amount3) %>%
  mutate("Source" = "Federal") %>%
  select(AUN, `Funding Classification`, TotalAmount, Source)

sta_revenue_22_23 <- revenue_22_23 %>%
  mutate("Funding Classification" = ifelse(AccountCode %in% c(7230, 7271, 7272), "Special Education Funding",
                                    ifelse(AccountCode %in% c(7600), "Nutrition Funding and Training",
                                    ifelse(AccountCode %in% c(7506), "Block Grants",
                                    ifelse(AccountCode %in% c(7292), "Child Care and Development",
                                    ifelse(AccountCode %in% c(7330), "Medical Assistance",
                                    ifelse(AccountCode %in% c(7280), "Adult Education",
                                    ifelse(AccountCode %in% c(7260, 7400), "Workforce Education",
                                    ifelse(AccountCode %in% c(7220, 7510), "Vocational Education",
                                    ifelse(AccountCode %in% c(7910, 7920, 7990), "School Improvement Funding",
                                    ifelse(AccountCode %in% c(7120), "Low Income Assistance", "check"))))))))))) %>%
  filter(`Funding Classification` != "check") %>%
  mutate(Amount2 = ifelse(is.na(Amount2), 0, Amount2),
         Amount3 = ifelse(is.na(Amount3), 0, Amount3),
         TotalAmount = Amount1 + Amount2 + Amount3) %>%
  mutate("Source" = "State") %>%
  select(AUN, `Funding Classification`, TotalAmount, Source)

sta_fed_rev <- rbind(fed_revenue_22_23, sta_revenue_22_23) %>%
  filter(`Funding Classification` %in% sta_revenue_22_23$`Funding Classification`) %>%
  group_by(`Funding Classification`, AUN, Source) %>%
  reframe("tot_rev" = sum(TotalAmount)) %>%
  merge(., adm_22_23, by = "AUN") %>%
  select(-wadm) %>%
  mutate("rev_per_adm" = tot_rev / adm) %>%
  select(-tot_rev, -adm) %>%
  pivot_wider(names_from = "Source", values_from = "rev_per_adm")
  

sta_fed_rev[is.na(sta_fed_rev)] <- 0

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