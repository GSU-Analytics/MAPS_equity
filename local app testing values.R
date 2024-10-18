#App testing values

current_nicename <- function() "Total Govt Appropriations as % Total Revenue"
current_nicename <- function() "Tuition and Fees as % Total Revenue"


input <- NULL
input$dname <- "University of Utah (230764)"
input$sector <- "Public, 4-year or above"
input$subscorePlot_click$y <- 27

current_feature <- function() {
  features %>%
    filter(nice_name == current_nicename()) %>% 
    filter(control_of_institution == (fsums %>% 
                                        filter(dname == input$dname) %>% 
                                        distinct(control_of_institution) %>% 
                                        pull(control_of_institution)))
}

raw_features_data <- function() {
  variables <- current_feature() %>% 
    pull(variable)
  
  raw_values %>% 
    filter(dname == input$dname) %>% 
    filter(variable %in% variables)
}

data <- raw_features_data() %>% 
  filter(variable == "tuition_and_fees_after_deducting_discounts_and_allowances")

