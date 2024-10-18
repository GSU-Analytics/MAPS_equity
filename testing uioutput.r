library(tidyverse)

ui <- fluidPage(
  uiOutput("row_mt")
)

server <- function(input, output) {
  output$row_mt <- renderUI({
    mt_list <- mtcars %>%
      rownames_to_column(var = "model") %>%
      rowwise() %>%
      group_split() %>%
      map(~{
        tagList(fluidRow(
          column(4,
                 .x$model),
          column(4,
                 .x$mpg),
          column(4, 
                 mtcars %>% 
                   filter(cyl == .x$cyl) %>% 
                   ggplot(aes(x = mpg, y = cyl)) + geom_point())
        ),
        br()
        )

      })

    tagList(mt_list)
  })
}

shinyApp(ui, server)

# 
# features <- read_rds("./data/features description.rds")
# fsums <- read_rds("./data/financial weighted and total scores.rds")
# 
# selected_feature <- features %>%
#   filter(nice_name == "Total Govt Appropriations as % Total Revenue") %>%
#   filter(control_of_institution == fsums %>% filter(dname == "University of Utah (230764)") %>% distinct(control_of_institution) %>% pull(control_of_institution))
# 
# ui <- fluidPage(
#   uiOutput("feature_row")
# )
# 
# server <- function(input, output) {
#   output$feature_row <- renderUI({
# 
#     mylist <- selected_feature %>%
#       rowwise() %>%
#       group_split() %>%
#       map(~{
#       tagList(fluidRow(
#         column(4, .x$var_title),
#         column(4, .x$description)
#         ),
#       br()
#       )
#     })
# 
#     tagList(mylist)
# 
#   })
# }
# shinyApp(ui, server)
