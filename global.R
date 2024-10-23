# DONE: combine race-pell-compl-enrollment dataset into on.
# DONE: School profile, financial index, pell-compl-enrollment three dataset system

# load libraries ----
library(tidyverse)
library(googlesheets4)
#library(sf)


library(shiny)
library(tidyverse)
library(glue)
library(sorensonimpact)
library(ggnewscale)
library(ggrepel)
library(dplyr)
library(scales)
library(plotly)
library(shinyWidgets)
library(ggridges)
library(forcats)
library(reshape2)
library(tidyverse)
library(tidymodels)
library(shinydashboardPlus)
library(highcharter)
library(shinyhelper)
library(shinyBS)
library(leaflet)
library(highcharter)
library(shinyWidgets)
library(htmltools)
library(DT)
library(shinyjs)
library(V8)
library(lubridate)
library(shinybusy)
library(shinycssloaders)
library(echarts4r)
library(reactable)
library(crosstalk)
library(ggcharts)
library(sparkline)
library(shiny.fluent)
library(arrow)
library(bslib)
library(devtools)
#devtools::install_github("deepanshu88/summaryBox", force = TRUE)
#devtools::install_github("Appsilon/shiny.react")
#devtools::install_github("Appsilon/shiny.fluent")
library(summaryBox)



#setwd("./share/financial model/Gong_takeover/")
#setwd("G:/.shortcut-targets-by-id/1dQehzxUw3rDFvGrx8xxSB-lvmANNun4i/")
#setwd("https://drive.google.com/drive/folders/1dQehzxUw3rDFvGrx8xxSB-lvmANNun4i?usp=sharing")
#setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1dQehzxUw3rDFvGrx8xxSB-lvmANNun4i")

options(scipen = 999)
#efa_fsums_pell_compl_profile_equity<-arrow::read_parquet("./data/efa_fsums_pell_compl_profile_equity.parquet") %>% ungroup()
#ipeds_dictionary<-arrow::read_parquet("./data/data_dictionary.parquet")%>% ungroup()
#long_ft <- arrow::read_parquet("./data/financial features - long.parquet")%>% ungroup()
#fsums <- arrow::read_parquet("./data/financial weighted and total scores.parquet") %>% ungroup()
#raw_values <- arrow::read_parquet("./data/raw_values.parquet")%>% ungroup()
#features<-arrow::read_parquet("./data/features description.parquet")%>% ungroup()

efa_fsums_pell_compl_profile_equity <- readRDS("./data/efa_fsums_pell_compl_profile_equity_dash.rds") %>% ungroup()


ipeds_dictionary<-readRDS("./data/data_dictionary.rds")%>% ungroup()
long_ft <- readRDS("./data/financial features - long.rds")%>% ungroup()
fsums <- readRDS("./data/financial weighted and total scores_new.rds") %>% ungroup()
raw_values <- readRDS("./data/rawvalues.rds")%>% ungroup()
features<-readRDS("./data/features description.rds")%>% ungroup()



features<-readRDS("./data/features description.rds")%>% ungroup()

raw_feature_slope_plot <- function(data,input_dname,input_var) {
  
  varscale <- unique(data$varscale)
  
  #ytitle <- unique(data$var_title)
  
  #This seems weird to me is it limiting only to harvard?
  p <- raw_values %>%
    filter(dname=="Harvard University (166027)") %>%
    filter(variable=="total_assets") %>%
    arrange(year) %>% 
    hchart("line",
           hcaes(x=year, y=value), 
           animation=T, 
           allowPointSelect=T,
           connectorWidth=4,
           marker=list(radius=5)
    )
  
  ggplot(aes(x = year, y = value)) +
    geom_point() +
    geom_smooth(method = "lm", formula = "y~x") +
    xlab("Year") + ylab(NULL) +
    expand_limits(y = 0) +
    scale_x_continuous(labels = function(x) str_replace(x, "^20", "\\'"))
  
  if(varscale == "si_scale_big_dollar") {
    p + scale_y_continuous(labels = si_scale_big_dollar)
  } else if(varscale == "scale_si_unit") {
    p + scale_y_continuous(labels = scale_si_unit())
  }
  
}

#profile <- read_rds("./data/profile.rds")
#features <- read_rds("./data/features description.rds") 
#efa_fsums_total <- read_rds("./data/efa_fsums_total.rds")

#profile_efa_fsums_total <- profile %>%
#  left_join(efa_fsums_total %>% filter(pell=="pell_count_sum_per_inst") %>%
#            distinct(unitid, ihe_name, pell_pct_per_inst))

#equity_compl<-read_rds("./data/equity_index.rds") %>%
#  select(unitid, institution_entity_name, sector_of_institution, race, completers_pct_per_school_race) %>%
#  distinct() %>%
#  left_join(profile %>% select(unitid, institution_entity_name=ihe_name, dname))

# Grand completion per inst
#grand_compl_per_inst <- profile %>%
#  select(dname, grand_completion_rate) %>%
#  mutate(grand_non_completion_rate=1-grand_completion_rate) %>%
#  pivot_longer(-1, names_to="completion", values_to="value") %>%
#  mutate(value=round(value,2))

#equity_enroll<-read_rds("./data/equity_index.rds") %>%
#  select(unitid, institution_entity_name, sector_of_institution, race, contains("enrol")) %>%
#  distinct(unitid, institution_entity_name, race, enrollment_pct_per_school_race) %>%
#  left_join(profile %>% select(unitid, institution_entity_name=ihe_name, dname)) %>%
#  mutate(enrollment_pct_per_school_race=round(enrollment_pct_per_school_race,3)) %>%
#  mutate(race=case_when(race=="american_indian_or_alaska_native"~ "AIAN",
#                        race=="asian"~"Asian",
#                        race=="black_or_african_american"~"Black",
#                        race=="hispanic_or_latino"~"Hispanic",
#                        race=="native_hawaiian_or_other_pacific_islander"~"Others",
#                        race=="race_ethnicity_unknown"~"Others",
#                        race=="two_or_more_races"~"Others",
#                        race=="white"~"White")) %>%
#  ungroup() %>%
#  group_by(dname,race) %>%
#  summarise(enrollment_pct_per_school_race=sum(enrollment_pct_per_school_race,na.rm=T)) 

# Pell grant graduation rate
#pell_compl<- read_rds("./data/gr_by_pell.rds") 

#profile_pell_compl<- profile %>% 
#  left_join(pell_compl %>% select(unitid, year, sector, pell_compl_pct,non_pell_compl_pct)) %>%
#  pivot_longer(c(pell_compl_pct,non_pell_compl_pct),names_to="pell_compl", values_to = "gr_pct") %>%
#  mutate(gr_pct=round(gr_pct,2))

# Completion rate by race
#compl_per_race<-read_rds("./data/compl_per_race.rds") %>%
#  mutate(per_race_non_completion_rate=1-per_race_completion_rate) %>%
#  pivot_longer(4:5, names_to="completion", values_to="value") %>%
#  left_join(profile %>% select(unitid, dname))


# fsums drilldown
#fsums<-fsums %>% 
#  mutate(drilldown=tolower(category.x)) %>% 
#  ungroup() %>%
#  group_by(dname,category) %>%
#  mutate(score_per_inst_category=mean(weighted_value,na.rm=T))

theme_set(theme_minimal(base_size = 18))

#fsums$institution_entity_name <- iconv(fsums$institution_entity_name, 'utf-8', 'ascii', sub='')
#efa_fsums_total$institution_entity_name <- iconv(efa_fsums_total$institution_entity_name, 'utf-8', 'ascii', sub='')


# Functions ---------------------------------------------------------------


text_describe_feature <- function(x) {
  statement <- " is calculated as: "
  numerator <- x %>% filter(fraction_part == "numerator")
  denom <- x %>% filter(fraction_part == "denominator") %>%
    distinct(variable, var_title, numerator_operation)
  
  if(nrow(numerator) == 1 & nrow(denom) == 0) {
    statement <- paste0(" only involves a single variable.")
    return(statement)
  }
  
  for(i in 1:nrow(numerator)) {
    statement <- paste0(statement, numerator$var_title[i])
    if(i != nrow(numerator) & nrow(numerator) != 1) statement <- paste0(statement, " ", numerator$numerator_operation[i], " ")
  } 
  
  
  if(nrow(denom) > 0) {
    statement <- paste0(statement, " divided by ")
    for(j in 1:nrow(denom)) {
      statement <- paste0(statement, denom$var_title[j])
      if(j != nrow(denom) & nrow(denom) != 1) statement <- paste0(statement, " ", denom$numerator_operation[j])
    }
  }
  return(statement)
  
  
  # statement %>% 
  #   str_remove(" is calculated as: ") %>% 
  #   str_replace_all("plus", "+") %>% 
  #   str_replace_all("minus", "-") %>% 
  #   str_replace_all("divided by", )
  
}

raw_feature_slope_plot <- function(data) {
  
  varscale <- unique(data$varscale)
  
  #ytitle <- unique(data$var_title)
  
  p <- data %>%
    arrange(year) %>% 
    ggplot(aes(x = year, y = value)) +
    geom_point() +
    geom_smooth(method = "lm", formula = "y~x") +
    xlab("Year") + ylab(NULL) +
    expand_limits(y = 0) +
    scale_x_continuous(labels = function(x) str_replace(x, "^20", "\\'"))
  
  if(varscale == "si_scale_big_dollar") {
    p + scale_y_continuous(labels = si_scale_big_dollar)
  } else if(varscale == "scale_si_unit") {
    p + scale_y_continuous(labels = scale_si_unit())
  }
  
}

#highchart() %>%
#hc_xAxis(categories = unique(parallel_chart$var_names)) %>%
# AIAN
#  hc_add_series(
#    data = parallel_chart %>% 
#      ungroup() %>%
#      filter(race=="AIAN") %>%
#      select(pct_count_values) %>% 
#      pull(pct_count_values),
#    type = "spline",
#    color = "#9e9947"
#  ) %>%
#  hc_yAxis

# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}
