# Author: Jonathan Zadra
# Created: Fri Jul 16 12:29:20 2021
# Description: Combine financial scores sectors for dashboard

# Modification: Byoung-gyu Gong
# Modified: Wed Oct 13 23:35:00 2021
# Description: Added graduation rate data to the profile data
options(scipen = 999)

library(tidyverse)
library(siverse)
library(conflicted)
library(fuzzyjoin)
library(numform)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

source("~/Google Drive/maps_project/model/financial models/new financial models - functions.R")

si_google_drive_path_fix()
if(!exists("hd")) hd <- ipeds_load("hd")
if(!exists("efa")) efa <- ipeds_load("efa")
if(!exists("ic_ay")) ic_ay <- ipeds_load("ic_ay")
if(!exists("ipeds_dictionary")) ipeds_dictionary <- ipeds_load("ipeds_dictionary")

pnfp_long_ft <- read_rds("~/Google Drive/SI/DataScience/data/maps_project/modified_data/financial model/private not-for-profit financial features - long.rds")
pnfp_fscore <- read_rds("~/Google Drive/SI/DataScience/data/maps_project/modified_data/financial model/private not-for-profit financial raw subscores.rds")
pnfp_fsums <- read_rds("~/Google Drive/SI/DataScience/data/maps_project/modified_data/financial model/private not-for-profit financial weighted and total scores.rds")

pfp_long_ft <- read_rds("~/Google Drive/SI/DataScience/data/maps_project/modified_data/financial model/private for-profit financial features - long.rds")
pfp_fscore <- read_rds("~/Google Drive/SI/DataScience/data/maps_project/modified_data/financial model/private for-profit financial raw subscores.rds")
pfp_sums <- read_rds("~/Google Drive/SI/DataScience/data/maps_project/modified_data/financial model/private for-profit financial weighted and total scores.rds")


pub_long_ft <- read_rds("~/Google Drive/SI/DataScience/data/maps_project/modified_data/financial model/public financial features - long.rds")
pub_fscore <- read_rds("~/Google Drive/SI/DataScience/data/maps_project/modified_data/financial model/public financial raw subscores.rds")
pub_fsums <- read_rds("~/Google Drive/SI/DataScience/data/maps_project/modified_data/financial model/public financial weighted and total scores.rds")

#equity<-readRDS("~/Google Drive/SI/DataScience/data/maps_project/modified_data/equity_index.rds")
completion<-read_rds("~/Google Drive/SI/DataScience/data/maps_project/modified_data/completion.rds")

long_ft <- bind_rows(pnfp_long_ft, pfp_long_ft, pub_long_ft)
fscore <- bind_rows(pnfp_fscore, pfp_fscore, pub_fscore)
fsums <- bind_rows(pnfp_fsums, pfp_sums, pub_fsums)

long_ft <- long_ft %>% 
  filter(!is.na(value)) %>% 
  mutate(dname = glue("{institution_entity_name} ({unitid})"))

fscore <- fscore %>% 
  filter(!is.na(value)) %>% 
  left_join(hd %>% filter(year == 2019) %>% select(unitid, institution_entity_name)) %>% 
  mutate(dname = glue("{institution_entity_name} ({unitid})"))

fsums <- fsums %>% 
  filter(!is.na(weighted_value)) %>% 
  mutate(dname = glue("{institution_entity_name} ({unitid})"))




fsums <- fsums %>% 
  distinct(dname, sector, total) %>% 
  group_by(sector) %>% 
  mutate(pct_rank_raw = percent_rank(total),
  #       pct_rescale_total=rescale(total)) %>%
         pct_rank = round(pct_rank_raw * 100, 0), 
         pct_rank = suppressWarnings(f_ordinal(pct_rank))) %>% #warnings are for values below 0. Get 0th anyways.
  ungroup() %>% 
  right_join(fsums) %>% 
  ungroup() %>%
  #group_by(sector, nice_name) %>%
  #mutate(pct_rank_subitem=percent_rank(weighted_value),
  #       rescale_subitem=rescale(weighted_value),
  #       rank_subitem=rank(weighted_value)) %>%
  #ungroup() %>%
  #group_by(dname) %>%
  #mutate(pct_rank_total=mean(pct_rank_subitem, na.rm=T)) %>%
  #       pct_rescale_total=mean(pct_rescale_subitem, na.rm=T),
  #       rank_total=mean(rank_subitem, na.rm=T))
         #pct_rank_subitem=round(pct_rank_subitem * 100, 0),
         #pct_rank_subitem=suppressWarnings(f_ordinal(pct_rank_subitem))) %>%
  left_join(hd %>% filter(year == max(year)) %>% select(sector, control_of_institution) %>% distinct())
  
# Dashboard ready formatting
  #. -----. rename code for sectors
fsums <- fsums %>%
  mutate(sector=str_replace(sector, "Private for-profit", "Proprietary"),
         sector=str_replace(sector, "Private not-for-profit", "Private")) %>%
  #. -----. Create column(score_per_inst_category) to be ready for drill-down graph
  mutate(drilldown=tolower(category)) %>% 
  ungroup() %>%
  group_by(dname,category) %>%
  mutate(score_per_inst_category=mean(weighted_value,na.rm=T)) %>%
  mutate(pct_rank_pct = paste0(pct_rank, " Percentile")) %>%
  mutate(total=round(total, 3)) %>%
  mutate(score_total=paste0("Score: ", total))



# Names Table -------------------------------------------------------------

names_join_table <- fuzzy_left_join(fsums %>% distinct(name, nice_name), long_ft %>% distinct(name, nice_name), match_fun = function(fdsa, asdf) {str_detect(fdsa, asdf)}) %>% 
  rename(nice_name_fsums = nice_name.x, nice_name_long_ft = nice_name.y, name_fsums = name.x, name_long_ft = name.y)

names_join_table <- names_join_table %>% ungroup() %>% select(-dname) %>% distinct()

fsums <- fsums %>% left_join(names_join_table %>% deselect(nice_name_fsums), by = c("name" = "name_fsums")) %>%
  mutate(weighted_value=round(weighted_value,3)) %>%
  mutate(score_per_inst_category=round(score_per_inst_category,3))

long_ft <- long_ft %>% left_join(names_join_table %>% deselect(nice_name_long_ft), by = c("name" = "name_long_ft"))


# Profile -----------------------------------------------------------------


profile <- hd %>% 
  ungroup() %>%
  group_by(unitid) %>%
  filter(year == max(year)) %>%
  select(unitid,
         year,
         ihe_name = institution_entity_name,
         city = city_location_of_institution,
         state = state_abbreviation_label) %>% 
  left_join(efa %>% 
              ungroup() %>%
              group_by(unitid) %>%
              filter(year == max(year)) %>% 
              select(unitid, 
                     year,
                     enroll_ft_ug = full_time_students_undergraduate_total_grand_total,
                     enroll_ft_grad = full_time_students_graduate_grand_total)) %>% 
  left_join(ic_ay %>% 
              ungroup() %>%
              group_by(unitid) %>%
              filter(year == max(year)) %>% 
              select(unitid, 
                     year,
                     # Undergraduate
                     in_state_average_tuition_for_full_time_undergraduates,
                     out_of_state_average_tuition_for_full_time_undergraduates,
                     # Graduate
                     in_state_average_tuition_full_time_graduates,
                     out_of_state_average_tuition_full_time_graduates
                     )) %>% 
  left_join(completion %>% 
              ungroup %>%
              group_by(unitid) %>%
              filter(year == max(year)) %>% 
              select(unitid, 
                     year,
                     grand_completion_rate) %>%
              distinct(unitid,grand_completion_rate)) %>%
  left_join(fsums %>% distinct(unitid, dname)) %>%
  filter(!is.na(dname)) %>%
  mutate(dname=case_when(str_detect(dname,"NA \\(")~glue('{ihe_name} ({unitid})'), TRUE~as.character(dname)))

write_rds(long_ft, "~/Google Drive/maps_project/share/financial model/Gong_takeover/data/financial features - long.rds")
write_rds(long_ft, "~/Google Drive/SI/DataScience/data/maps_project/modified_data/financial model/financial features - long.rds")
write_rds(long_ft, "./data/financial features - long.rds")
write_rds(long_ft, "./data/financial features - long.rds")

write_rds(fscore, "~/Google Drive/maps_project/share/financial model/Gong_takeover/data/financial raw subscores.rds")
write_rds(fsums, "~/Google Drive/maps_project/share/financial model/Gong_takeover/data/financial weighted and total scores.rds")
write_rds(fsums, "~/Google Drive/SI/DataScience/data/maps_project/modified_data/financial model/financial weighted and total scores.rds")
write_rds(fsums, "./data/financial weighted and total scores.rds")

write_rds(profile,"~/Google Drive/SI/DataScience/data/maps_project/modified_data/financial model/profile.rds")
write_rds(profile, "~/Google Drive/maps_project/share/financial model/Gong_takeover/data/profile.rds")
write_rds(profile, "./share/financial model/Gong_takeover/data/profile.rds")
write_rds(profile, "./data/profile.rds")

#profile<-readRDS("/Volumes/GoogleDrive/My Drive/maps_project/model/race equity index/Shiny_app copy/data/profile.rds")


# Manually create tables of feature creation. -----------------------------
  #Cut and paste from the feature creation mutate
public_features <- edt("unitid = unitid, 
            year = year, 
            sector = sector, 
            enrollment_grad_only = enrollment_grad_only, 
            enrollment = enrollment, 
            retention_rate = retention_rate,#can be transmute when done with diagnostics
            net_income = change_in_net_position_during_the_year,
            
            tuition_and_fees_as_pct_total_rev = tuition_and_fees_after_deducting_discounts_and_allowances / total_operating_and_nonoperating_revenues,
         
         total_govt_appropriations_as_pct_total_rev = sum(federal_appropriations, state_appropriations, local_appropriations_education_district_taxes_and_similar_support, na.rm = T) / total_operating_and_nonoperating_revenues,
         
         govt_grants_contracts_as_pct_total_rev = sum(federal_operating_grants_and_contracts, state_operating_grants_and_contracts, local_private_operating_grants_and_contracts, federal_nonoperating_grants, state_nonoperating_grants, local_nonoperating_grants, investment_income, other_nonoperating_revenues, local_operating_grants_and_contracts, private_operating_grants_and_contracts, na.rm = T) / total_operating_and_nonoperating_revenues,
         
         private_gifts_as_pct_total_rev = gifts_including_contributions_from_affiliated_organizations / total_operating_and_nonoperating_revenues,
         
         sales_investment_income_as_pct_total_rev = sum(sales_and_services_of_auxiliary_enterprises, sales_and_services_of_hospitals, independent_operations, other_sources_operating, sales_and_services_of_educational_activities, na.rm = T) / total_operating_and_nonoperating_revenues,
         
         revenue_per_student = total_operating_and_nonoperating_revenues / enrollment, #enroll already adjusted for grad only.
         
         discounts_and_allowances_applied_to_tuition_and_fees_per_student = discounts_and_allowances_applied_to_tuition_and_fees / enrollment, #enrollment is already adjusted for grad only
         
         instruction_exp_as_pct_total_exp = instruction_current_year_total / total_expenses_and_deductions_current_year_total,
         
         academic_support_as_pct_total_exp = academic_support_current_year_total / total_expenses_and_deductions_current_year_total,
         
         student_service_as_pct_total_exp = student_services_current_year_total / total_expenses_and_deductions_current_year_total,
         
         scholarships_fellowships_exp_as_pct_total_exp = scholarships_and_fellowships_expenses_current_year_total / total_expenses_and_deductions_current_year_total,
         
         total_debt_to_total_assets_ratio = (total_liabilities - net_pension_liability) / total_assets,
         
         total_current_assets_as_pct_total_exp = total_current_assets / total_expenses_and_deductions_current_year_total,
         
         value_of_endowment_as_pct_total_exp = value_of_endowment_assets_at_the_end_of_the_fiscal_year / total_expenses_and_deductions_current_year_total,
         
         restricted_assets_to_total_assets_ratio = sum(restricted_nonexpendable, restricted_expendable, na.rm = T) / total_assets")



pfp_features <- edt("unitid = unitid, 
            year = year, 
            sector = sector,  
            enrollment_grad_only = enrollment_grad_only, 
            enrollment = enrollment, 
            retention_rate = retention_rate, #can be transmute when done with diagnostics
            net_income = net_income, 
            
            tuition_and_fees_as_pct_total_rev = tuition_and_fees / total_revenues_and_investment_return,
         
         total_student_grants_as_pct_total_rev = total_student_grants / total_revenues_and_investment_return,
         
         govt_appropriations_grants_as_pct_total_rev = sum(federal_appropriations_grants_and_contracts, state_and_local_appropriations_grants_and_contracts, private_gifts_grants_and_contracts, federal_appropriations, federal_grants_and_contracts, state_appropriations, state_grants_and_contracts, local_government_appropriations, local_government_and_contracts, na.rm = T) / total_revenues_and_investment_return,
         
         sales_or_other_earned_revenue_as_pct_total_rev = sum(investment_income_and_investment_gains_losses_included_in_net_income, sales_and_services_of_educational_activities, sales_and_services_of_auxiliary_enterprises, other_revenue, na.rm = T) / total_revenues_and_investment_return,
 
 revenue_per_student = total_revenues_and_investment_return / enrollment,
 
 discounts_and_allowances_applied_to_tuition_and_fees_per_student = discounts_and_allowances_applied_to_tuition_and_fees / enrollment,
 
 instruction_exp_as_pct_total_exp = instruction_total_amount / total_expenses_total_amount,

 academic_support_as_pct_total_exp = academic_support_total_amount / total_expenses_total_amount,
 
 student_service_as_pct_total_exp = student_service_total_amount / total_expenses_total_amount,
 
 institutional_support_as_pct_total_exp = institutional_support_total_amount / total_expenses_total_amount,
 
 total_debt_to_total_assets_ratio = total_liabilities / total_assets")

pnotfp_features <- edt("unitid = unitid, 
            year = year, 
            sector = sector, 
            enrollment_grad_only = enrollment_grad_only, 
            enrollment = enrollment, 
            retention_rate = retention_rate,#can be transmute when done with diagnostics
            
            net_income = total_revenues_and_investment_return_total - total_expenses_total_amount,
            
            tuition_and_fees_as_pct_total_rev = tuition_and_fees_total / total_revenues_and_investment_return_total,
            
            total_student_grants_as_pct_total_rev = total_student_grants / total_revenues_and_investment_return_total,
         
         govt_appropriations_as_pct_total_rev = sum(federal_appropriations_total, federal_appropriations_unrestricted, federal_appropriations_temporarily_restricted, federal_appropriations_permanently_restricted, state_appropriations_total, state_appropriations_unrestricted, state_appropriations_temporarily_restricted, state_appropriations_permanently_restricted, local_appropriations_total, local_appropriations_unrestricted, local_appropriations_temporarily_restricted, local_appropriations_permanently_restricted, na.rm = T) / total_revenues_and_investment_return_total,
         
         govt_grants_contracts_as_pct_total_rev = sum(federal_grants_and_contracts_total, federal_grants_and_contracts_unrestricted, federal_grants_and_contracts_temporarily_restricted, federal_grants_and_contracts_pemanently_restricted, state_grants_and_contracts_total, state_grants_and_contracts_unrestricted, state_grants_and_contracts_temporarily_restricted, state_grants_and_contracts_permanently_restricted, local_grants_and_contracts_total, local_grants_and_contracts_unrestricted, local_grants_and_contracts_temporarily_restricted, local_grants_and_contracts_permanently_restricted, na.rm = T) / total_revenues_and_investment_return_total,
         
         private_gifts_as_pct_total_rev = sum(private_gifts_total, contributions_from_affiliated_entities_total, na.rm = T) / total_revenues_and_investment_return_total,
         
         sales_other_earned_rev_as_pct_total_rev = sum(investment_return_total, sales_and_services_of_educational_activities_total, sales_and_services_of_auxiliary_enterprises_total, hospital_revenue_total, independent_operations_revenue_total, na.rm = T) / total_revenues_and_investment_return_total,
         
         revenue_per_student = total_revenues_and_investment_return_total / enrollment, #enroll already adjusted for grad only.
         
         allowances_applied_to_tuition_and_fees_per_student = allowances_applied_to_tuition_and_fees / enrollment, #enrollment is already adjusted for grad only
         
         instruction_exp_as_pct_total_exp = instruction_total_amount / total_expenses_total_amount,
         
         academic_support_as_pct_total_exp = academic_support_total_amount / total_expenses_total_amount,
         
         student_service_as_pct_total_exp = student_service_total_amount / total_expenses_total_amount,
         
         institutional_support_as_pct_total_exp = institutional_support_total_amount / total_expenses_total_amount,
         
         
         
         total_debt_to_total_assets_ratio = total_liabilities / total_assets,
         
         restricted_assets_to_total_assets_ratio = total_restricted_net_assets / total_net_assets,
         
         value_of_endowment_as_pct_total_exp = value_of_endowment_assets_at_the_end_of_the_fiscal_year / total_expenses_total_amount")

features <- bind_rows("Public" = public_features, "Private for-profit" = pfp_features, "Private not-for-profit" = pnotfp_features, .id = "control_of_institution") %>% 
  mutate(survey_group = case_when(control_of_institution == "Public" ~ "f_f1a",
                           control_of_institution == "Private for-profit" ~ "f_f3",
                           control_of_institution == "Private not-for-profit" ~ "f_f2")) %>% 
  pivot_longer(-c(control_of_institution, derived, sequence, survey_group, numerator_operation, sequence), names_to = "fraction_part", values_to = "variable") %>% 
  left_join(ipeds_dictionary %>% select(survey_group, variable = var_column, var_title, description)) %>% 
  mutate(description = str_squish(description)) %>% 
  left_join(fsums %>% distinct(nice_name, name_long_ft), by = c("derived" = "name_long_ft")) %>% 
  filter(!is.na(variable)) %>% 
  filter(derived != "enrollment_grad_only") 

additional_features <- read_excel("~/Google Drive/SI/DataScience/data/maps_project/modified_data/financial model/features additions.xlsx")

features <- features %>% 
  filter(!str_detect(derived, "per_student|enroll|retention")) %>% 
  bind_rows(additional_features)

features %>% 
  mutate(classification = case_when(str_detect(nice_name, "Change Over Time") ~ glue("The slope of the derived variable {str_to_lower(nice_name)} was then calculated using a simple linear regression model with year (centered) as the independent (predictor) variable.")))




# features <- features %>% 
#   mutate(varscale = case_when(str_detect(variable, "enrollment|retention") ~ "scale_si_unit",
#                               TRUE ~ "si_scale_big_dollar"))

write_rds(features, "~/Google Drive/maps_project/share/financial model/Gong_takeover/data/features description.rds")
write_rds(features, "./data/features description.rds")



# Raw values --------------------------------------------------------------

pub_raw <- read_rds("~/Google Drive/SI/DataScience/data/maps_project/modified_data/financial model/public_raw.rds")
pnfp_raw <- read_rds("~/Google Drive/SI/DataScience/data/maps_project/modified_data/financial model/pnfp_raw.rds")
pfp_raw <- read_rds("~/Google Drive/SI/DataScience/data/maps_project/modified_data/financial model/pfp_raw.rds")

raw_values <- bind_rows(
  pub_raw %>% 
    deselect(institution_is_active_in_current_year, multi_institution_or_multi_campus_organization, name_of_multi_institution_or_multi_campus_organization, status_of_institution, year_institution_was_deleted_from_ipeds, starts_with("flag")) %>% 
    pivot_longer(-c(year, unitid, ien, sector), names_to = "variable"),
  
  pnfp_raw %>% 
    deselect(institution_is_active_in_current_year, multi_institution_or_multi_campus_organization, name_of_multi_institution_or_multi_campus_organization, status_of_institution, year_institution_was_deleted_from_ipeds, starts_with("flag")) %>% 
    pivot_longer(-c(year, unitid, ien, sector), names_to = "variable"),
  
  pfp_raw %>% 
    deselect(institution_is_active_in_current_year, multi_institution_or_multi_campus_organization, name_of_multi_institution_or_multi_campus_organization, status_of_institution, year_institution_was_deleted_from_ipeds, starts_with("flag")) %>% 
    pivot_longer(-c(year, unitid, ien, sector), names_to = "variable")
)

raw_values <- raw_values %>% 
  mutate(dname = glue("{ien} ({unitid})")) %>% 
  mutate(varscale = case_when(str_detect(variable, "enrollment|retention") ~ "scale_si_unit",
                              TRUE ~ "si_scale_big_dollar"))

write_rds(raw_values, "~/Google Drive/maps_project/share/financial model/Gong_takeover/data/raw_values.rds")
write_rds(raw_values, "./data/raw_values.rds")



# 
# #Diagnosing issues with unitid name changes and merges ----
# a <- fsums %>% left_join(names_join_table, by = c("name" = "name_fsums"))
# 
# long_ft %>% left_join(a, by = c("name" = "name_long_ft"))
# 
# 
# #Problem:  Schools with same names but different unitids will cause problems in the selection list.
# hd %>% filter(year >= 2014, !str_detect(sector, "less")) %>% select(unitid, year, city_location_of_institution, state_abbreviation_label, institution_entity_name) %>% distinct(unitid, city_location_of_institution, state_abbreviation_label, institution_entity_name, .keep_all = T) %>% get_dupes(institution_entity_name)
# 
# hd %>% idf(c(433420, 489928))
# 
# 
# hd %>% filter(year >= 2014, !str_detect(sector, "less")) %>% select(unitid, year, city_location_of_institution, state_abbreviation_label, institution_entity_name) %>% distinct(unitid, city_location_of_institution, state_abbreviation_label, institution_entity_name, .keep_all = T) %>% get_dupes(institution_entity_name, state_abbreviation_label, city_location_of_institution) %>% 
#   left_join(hd %>% select(unitid, year, unitid_for_merged_schools))
# 
# hd %>% filter(year >= 2014, !str_detect(sector, "less")) %>% select(unitid, year, city_location_of_institution, state_abbreviation_label, institution_entity_name) %>% distinct(unitid, city_location_of_institution, state_abbreviation_label, institution_entity_name, .keep_all = T) %>% get_dupes(institution_entity_name, state_abbreviation_label, city_location_of_institution) %>% 
#   left_join(hd %>% select(unitid, year, unitid_for_merged_schools)) %>% group_by(unitid) %>% mutate(nyear = n_distinct(year)) %>% ungroup
# 
# hd %>% filter(year >= 2014, !str_detect(sector, "less")) %>% 
#   group_by(unitid) %>% 
#   mutate(nyear = n_distinct(year)) %>% ungroup() %>% 
#   select(unitid, year, city_location_of_institution, state_abbreviation_label, institution_entity_name, status_of_institution, street_address_or_post_office_box, nyear) %>% distinct(unitid, city_location_of_institution, state_abbreviation_label, institution_entity_name, street_address_or_post_office_box, .keep_all = T) %>% get_dupes(institution_entity_name, state_abbreviation_label, city_location_of_institution, street_address_or_post_office_box) %>% distinct(unitid) %>% pull(unitid) -> mnames
# 
# asdf <- hd %>% idf(mnames) %>% filter(year >= 2014) %>% group_by(unitid) %>% 
#   mutate(nyear = n_distinct(year)) %>% ungroup() %>% select(unitid, year, nyear, institution_entity_name, city_location_of_institution, state_abbreviation_label,  unitid_for_merged_schools, status_of_institution, street_address_or_post_office_box) %>% arrange(institution_entity_name, city_location_of_institution, year) 
# 
# asdf %>% View
# 
# asdf %>%  filter(unitid_for_merged_schools != -2)

# More for the financial model
#pell_raw<-read_rds("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/maps_project/raw_data/ipeds/custom outputs/years combined unrecoded/sfa - 2006-2018.rds")
pell_org <- read_rds("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/maps_project/modified_data/pell_clean.rds") %>%
  filter(year>2013) %>%
  rename(pell_count_per_inst_year=number_of_undergraduate_students_awarded_pell_grants,
         pell_pct_per_inst_year=percent_of_undergraduate_students_awarded_pell_grants,
         inst_gr_avg_amt_per_inst_year=average_amount_of_institutional_grant_aid_awarded_to_full_time_first_time_undergraduates,
         pell_gr_avg_amt_per_inst_year=average_amount_of_pell_grant_aid_awarded_to_full_time_first_time_undergraduates)
# instructor percent
EQUITY<-readRDS(file="~/Google Drive/maps_project/model/race equity index/Shiny_app/data/equity_index.rds") %>%
  select(unitid, institution_entity_name, year, race, 
         instructor_count_per_sch_race_year, 
         instructor_percent,
         pop_count_per_state_race_year,
         statePOP_pct, #pop_pct_per_state_race_year
         pop_count_per_natl_race_year,
         natlPOP_pct_per_race_year) %>%#pop_pct_per_natl_race_year
  rename(instructor_count_per_inst_year_race="instructor_count_per_sch_race_year",
         instructor_pct_per_inst_year_race="instructor_percent",
         pop_count_per_state_year_race="pop_count_per_state_race_year",
         pop_pct_per_state_year_race="statePOP_pct",
         pop_count_per_natl_year_race="pop_count_per_natl_race_year",
         pop_pct_per_natl_year_race="natlPOP_pct_per_race_year") %>%
  mutate(race=case_when(race=="american_indian_or_alaska_native"~ "AIAN",
                        race=="asian"~"Asian",
                        race=="black_or_african_american"~"Black",
                        race=="hispanic_or_latino"~"Hispanic",
                        race=="native_hawaiian_or_other_pacific_islander"~"NHPI",
                        race=="race_ethnicity_unknown"~"Race Unknown",
                        race=="two_or_more_races"~"Two or More Races",
                        race=="white"~"White"))

# Another for efa_tidy_ext
efa_tidy_ext<-read_rds("~/Google Drive/SI/DataScience/data/maps_project/modified_data/efa_tidy_ext.rds")

# Add completion rate
completion<-read_rds("~/Google Drive/SI/DataScience/data/maps_project/modified_data/completion.rds")

# Add tuition per student
profile<-read_rds("~/Google Drive/SI/DataScience/data/maps_project/modified_data/financial model/profile.rds") %>%
  #select(c(1:9),dname) %>%
  distinct()

# To find the pell_compl data look at: /Volumes/GoogleDrive/My Drive/maps_project/modify/ipeds/IPEDs pell grant recipients.R
pell_compl<- read_rds("~/Google Drive/maps_project/share/financial model/Gong_takeover/data/gr_by_pell.rds") %>%
  select(unitid, year, pell_compl_pct,non_pell_compl_pct) %>%
  mutate(pell_non_comple_pct=1-pell_compl_pct,
         non_pell_non_compl_pct=1-non_pell_compl_pct)

# convert year into annual average - pell vs. non-pell count is based on first-time students, not including transfered students.
pell<-pell_org %>% 
  mutate(fulltime_firsttime_stu_total_per_inst_year=trunc(pell_count_per_inst_year/pell_pct_per_inst_year),
         nonpell_count_per_inst_year=trunc(fulltime_firsttime_stu_total_per_inst_year - pell_count_per_inst_year)) %>%
  select(unitid, year,pell_count_per_inst_year,pell_pct_per_inst_year,
         nonpell_count_per_inst_year,fulltime_firsttime_stu_total_per_inst_year) %>%
  mutate(nonpell_pct_per_inst_year=1-pell_pct_per_inst_year)
  #group_by(unitid, year) #%>%
  #summarise(pell_count_avg_per_inst=trunc(mean(pell_count_per_inst_year, na.rm=T)),
  #          pell_count_sum_per_inst=sum(pell_count_per_inst_year, na.rm=T),
  #          nonpell_count_avg_per_inst=trunc(mean(nonpell_count_per_inst_year, na.rm=T)),
  #          nonpell_count_sum_per_inst=trunc(sum(nonpell_count_per_inst_year, na.rm=T)),
  #          pell_pct_per_inst=mean(pell_pct_per_inst_year,na.rm=T),
  #          inst_gr_avg_amt_per_inst=mean(inst_gr_avg_amt_per_inst_year,na.rm=T),
  #          pell_gr_avg_amt_per_inst=mean(pell_gr_avg_amt_per_inst_year,na.rm=T))

efa_fsums<-efa_tidy_ext %>% 
  filter(gender=="Total"&year>2013) %>% 
  select(-gender) %>%
  mutate(race=case_when(race=="american_indian_or_alaska_native"~ "AIAN",
                        race=="asian"~"Asian",
                        race=="black_or_african_american"~"Black",
                        race=="hispanic_or_latino"~"Hispanic",
                        race=="native_hawaiian_or_other_pacific_islander"~"NHPI",
                        race=="race_ethnicity_unknown"~"Race Unknown",
                        race=="two_or_more_races"~"Two or More Races",
                        race=="white"~"White")) %>%
  ungroup() %>%
  group_by(unitid,year,race) %>%
  summarise(enrollment_count_per_inst_year_race=sum(enrolment_count)) %>%
  distinct() %>%
  ungroup() %>% 
  group_by(unitid,year) %>%
  mutate(enrollment_total_per_inst_year=sum(enrollment_count_per_inst_year_race)) %>%
  mutate(enrollment_pct_per_inst_year_race=(enrollment_count_per_inst_year_race/enrollment_total_per_inst_year)) %>%
  distinct() %>%
  # merging with FHS
  left_join(fsums %>% select(unitid, total, sector), by="unitid") %>% 
  distinct() %>%
  # Allocated the FHS score to each races in the inst-year by their pct in enrollment
  mutate(FHS_per_inst_year_race=(enrollment_count_per_inst_year_race*total)) %>%
  ungroup() %>%
  group_by(year,race) %>%
  mutate(enrollment_total_per_year_race=sum(enrollment_count_per_inst_year_race)) %>%
  #mutate(FHS_total_per_race=sum(FHS_per_race, na.rm=T)) %>%
  #mutate(FHS_per_race_mean=(FHS_total_per_race/enrollment_total_per_race)) %>%
  drop_na(total)

## Merge with efa_fsums and pell

efa_fsums_pell<-efa_fsums %>% 
  left_join(pell, by=c("unitid","year")) %>%
  left_join(pell_compl, by=c("unitid","year"))
  #ungroup() %>%
  #select(dname, unitid, sector, pell_count_sum_per_inst,nonpell_count_sum_per_inst,total) %>%
  #distinct() %>%
  #pivot_longer(3:4,names_to="pell",values_to="count") %>%
  #group_by(unitid) %>%
  #mutate(pell_sum_per_inst=sum(count, na.rm=T)) %>%
  #ungroup() %>%
  #mutate(pell_pct_per_inst=round((count/pell_sum_per_inst),2))

## Completion rate
# -----. grand_completion
compl1<-completion %>%
  filter(gender=="Total") %>%
  rename(adjusted_cohort_count_per_inst_year="grand_adjusted_cohort_count",
         completers_count_per_inst_year="grand_completers_count",
         adjusted_cohort_count_per_inst_year_race="adjusted_cohort_count",
         completers_count_per_inst_year_race="completers_count",
         adjusted_cohort_pct_per_inst_year_race="adjusted_cohort_pct",
         completers_pct_per_inst_year_race="completers_pct",
         completion_rate_per_inst_year_race="per_race_completion_rate",
         completion_rate_per_inst_year="grand_completion_rate") %>%
  mutate(race=case_when(race=="american_indian_or_alaska_native"~ "AIAN",
                        race=="asian"~"Asian",
                        race=="black_or_african_american"~"Black",
                        race=="hispanic_or_latino"~"Hispanic",
                        race=="native_hawaiian_or_other_pacific_islander"~"NHPI",
                        race=="race_ethnicity_unknown"~"Race Unknown",
                        race=="two_or_more_races"~"Two or More Races",
                        race=="white"~"White"))
  
compl2 <- compl1 %>%
  ungroup() %>%
  select(-cohort_year,-gender) %>%
  group_by(unitid,year,race) %>%
  summarise(completion_rate_per_inst_year=mean(completion_rate_per_inst_year, na.rm=T),
            completion_rate_per_inst_year_race=mean(completion_rate_per_inst_year_race, na.rm=T))

compl<-compl1 %>% 
  left_join(compl2, by=c("unitid","year","race")) %>%
  select(-contains(".y")) %>%
  rename(completion_rate_per_inst_year="completion_rate_per_inst_year.x",
         completion_rate_per_inst_year_race ="completion_rate_per_inst_year_race.x")

efa_fsums_pell_compl<-efa_fsums_pell %>%
  left_join(compl, by=c("unitid","year","race"))

efa_fsums_pell_compl_profile_equity <- efa_fsums_pell_compl %>%
  left_join(profile, by=c("unitid","year")) %>%
  
  # Create location column
  unite(col = location, city, state, sep = ", ") %>%
  
  # Create enrollment count per ug and grad
  mutate(enroll_ft_ug = {scales::comma(enroll_ft_ug) %>% str_replace(., "\\..*", "")}) %>% 
  mutate(enroll_ft_grad = {scales::comma(enroll_ft_grad) %>% str_replace(., "\\..*", "")}) %>%
  unite(enroll, enroll_ft_ug, enroll_ft_grad, sep = " | ") %>%
  mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race, 3)) %>%
  
  # Completion %
  mutate(non_completion_rate_per_inst_year=1-completion_rate_per_inst_year) %>%
  mutate(non_completion_rate_per_inst_year_race=1-completion_rate_per_inst_year_race) %>%
  mutate(completion_rate_per_inst_year=round(completion_rate_per_inst_year,2),
         non_completion_rate_per_inst_year=round(non_completion_rate_per_inst_year,2)) %>%
  mutate(completion_rate_per_inst_year_pct=scales::percent(round(completion_rate_per_inst_year,2))) %>%
  mutate(non_completion_rate_per_inst_year_pct=scales::percent(round(non_completion_rate_per_inst_year,2))) %>%
  
  # Tuition in dollar
  mutate(in_state_under = scales::dollar(in_state_average_tuition_for_full_time_undergraduates)) %>% 
  mutate(out_state_under = scales::dollar(out_of_state_average_tuition_for_full_time_undergraduates)) %>% 
  unite(tuition_under, in_state_under, out_state_under, sep = " | ") %>%
  mutate(in_state_grad = scales::dollar(in_state_average_tuition_full_time_graduates)) %>% 
  mutate(out_state_grad = scales::dollar(out_of_state_average_tuition_full_time_graduates)) %>% 
  unite(tuition_grad, in_state_grad, out_state_grad, sep = " | ") %>%
  
  # Pell pct
  mutate(pell_pct_per_inst_year_pct=scales::percent(pell_pct_per_inst_year)) %>%
  
  # FHI score
  mutate(total=round(total, 3)) %>%
  #select(-dname.y) %>%
  #rename(dname="dname.x") %>%
  rename("dname"=dname.x) %>%
  select(-dname.y) %>%
  
  # Combining with fsums
  right_join(fsums %>% distinct(dname,unitid), by="dname") %>%
  select(dname, unitid.x, ihe_name, location, sector, year, race, total, FHS_per_inst_year_race,
         contains("enroll"), 
         contains("pell"),
         contains("comple"),
         contains("tuition"),
         -unitid.y) %>%
  rename(unitid="unitid.x") %>%
  distinct() %>%

  # Combining with EQUITY
  right_join(EQUITY, by=c("unitid","year","race")) %>%
  
  # Trim state and city locations
  separate(location, into=c("city","state"), sep=",",remove=F) %>%
  mutate(state=str_trim(state)) %>% 
  
  # Race_long
  mutate("race_long" = race) %>% 
  mutate_at(c("race_long"), str_replace_all, pattern = "AIAN", replacement = "American Indian and Alaskan Native") %>% 
  mutate_at(c("race_long"), str_replace_all, pattern = "NHPI", replacement = "Native Hawaiian and Pacific Islander") %>% 
  
  # Pct display
  mutate(enrollment_pct_per_inst_year_race_2 = enrollment_pct_per_inst_year_race*100) %>% 
  mutate(instructor_pct_per_inst_year_race_2 = instructor_pct_per_inst_year_race*100) %>% 
  mutate(pop_pct_per_state_year_race_2 = pop_pct_per_state_year_race*100) %>% 
  mutate(pop_pct_per_natl_year_race_2 = pop_pct_per_natl_year_race*100) %>% 
  mutate(completers_pct_per_inst_year_race_2 = completers_pct_per_inst_year_race*100) %>% 
  mutate(completion_rate_per_inst_year_race_2 = completion_rate_per_inst_year_race*100)
  


#write_rds(efa_fsums_pell_compl_profile, "C:/Users/allso/Documents/Github Random Projects/MAPS_FHI_DASH\MAPS_FHI_DASH/data")
write_rds(efa_fsums_pell_compl_profile, "~/Google Drive/maps_project/share/financial model/Gong_takeover/data/efa_fsums_pell_compl_profile.rds")
write_rds(efa_fsums_pell_compl_profile, "~/Google Drive/MAPS_FHI_DASH/data/efa_fsums_pell_compl_profile.rds")
write_rds(efa_fsums_pell_compl_profile, "./data/efa_fsums_pell_compl_profile.rds")
write_rds(efa_fsums_pell_compl_profile_equity, "./data/efa_fsums_pell_compl_profile_equity.rds")
  
write_rds(compl_per_race, "~/Google Drive/maps_project/share/financial model/Gong_takeover/data/compl_per_race.rds")
arrow::write_parquet(efa_fsums_pell_compl_profile_equity, "./data/efa_fsums_pell_compl_profile_equity.parquet")

## Students enrolled(size)
#enroll_size<-efa_fsums %>% ungroup() %>%
#  select(unitid,enrollment_total) %>%
#  distinct()

## Cost of tuition or cumulative net price
if(!exists("ic_ay")) ic_ay <- ipeds_load("ic_ay")
ic_ay<-ic_ay %>%
  select(unitid, year, in_state_average_tuition_for_full_time_undergraduates) %>%
  group_by(unitid) %>%
  summarise(tuition_in_state_avg_per_inst=mean(in_state_average_tuition_for_full_time_undergraduates, na.rm=T))
# extract institution names
hd2 <- hd %>%
  select(unitid, institution_entity_name)
# Combining all information per inst
efa_fsums_total<- efa_fsums_pell %>%
  left_join(compl, by="unitid") %>%
  left_join(enroll_size, by="unitid") %>%
  left_join(ic_ay, by="unitid") %>%
  left_join(profile, by= "unitid") %>%
  left_join(hd2, by="unitid")


write_rds(efa_fsums_total, "~/Google Drive/maps_project/share/financial model/Gong_takeover/data/efa_fsums_total.rds")




######## Converting data in shinyapp datafolder to parquet format ########
read_rds("./data/features description.rds") %>% arrow::write_parquet("./data/features description.parquet")
read_rds("./data/financial features - long.rds") %>% arrow::write_parquet("./data/financial features - long.parquet")
read_rds("./data/financial weighted and total scores.rds") %>% arrow::write_parquet("./data/financial weighted and total scores.parquet")
read_rds("./data/raw_values.rds") %>% arrow::write_parquet("./data/raw_values.parquet")
read_rds("./data/data_dictionary.rds") %>% arrow::write_parquet("./data/data_dictionary.parquet")
