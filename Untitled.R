#Shiny.Run.R
setwd("/Volumes/GoogleDrive/My Drive/MAPS_FHI_DASH")
options(shiny.autoreload = TRUE)
shiny::runApp()
rstudioapi::viewer("http://127.0.0.1:4956")

#navbarPage(title = div(class = "lvl3_title", "Financial Health Indicators Index"), 
#           id = "navbar",
#           theme = "fhi_dash.css",
#           collapsible = TRUE,
#           inverse = FALSE,
#           windowTitle = FALSE,
#           position = "fixed-top",
#           footer = NULL,
#           header = NULL,
#           
#           # ----- Tab panel1. Home.



# -----.  Index
#tabPanel("Index Score Breakdown",
#         fluidRow(align="left",
#                  column(12,style="margin-left=0px",
#                         div(class="lvl2_title",
#                             htmlOutput("ihe_name"),
#                         ))
#         ),
         # -. School Location
#         fluidRow(
#           column(6,
#                  div(class="location",
#                      htmlOutput("ihe_location"))
#           )
#         ),
#         br(),
#         fluidRow(style='background-color: #e9b73f',
#                  align="left",
#                  div(class="lvl2_title_wh",
#                      "Financial Health Index Score")),
#         br(),
#         fluidRow(
#           column(4,style="margin-right:50px; margin-left:50px",
#                  
#                  fluidRow(style='padding:10px; font-size: 5rem; 
#                                                           font-weight: 900; color:#3e6d72',
#                           textOutput("ihe_fhiscore")),
#                  fluidRow(style='padding:10px; font-size: 2.5rem; color:black',
#                           "range: -0.2 – 0.2")
#           ),
#           column(5,style="margin-right:50px; margin-left:50px",
                  
#                  fluidRow(style='padding:10px; font-size: 5rem; 
#                                                           font-weight: 900; color:#3e6d72',
#                           textOutput("ihe_percentile")),
#                  fluidRow(style='padding:10px; font-size: 2.5rem; color:black',
#                           htmlOutput("ihe_percentile_info"))
#           )
#         ),
#         br(),
#         fluidRow(style='background-color: #e9b73f',
#                  align="left",
#                  div(class="lvl2_title_wh",
#                      "School Demographics Breakdown")),
#         br()
         #fluidRow(
         #highchartOutput("distlocationPlot")
         #)
#),

# -----. Compare
#tabPanel("Compare Schools",
#         fluidRow(align="left",
#                  column(12,style="margin-left=0px",
#                         div(class="lvl2_title",
#                             htmlOutput("ihe_name"),
#                         ))
#         ),
         # -. School Location
#         fluidRow(
#           column(6,
#                  div(class="location",
#                      htmlOutput("ihe_location"))
#           )
#         ),
#         br(),
#         fluidRow(style='background-color: #e9b73f',
#                  align="left",
#                  div(class="lvl2_title_wh",
#                      "Financial Health Index Score")),
#         br(),
#         fluidRow(
#           column(4,style="margin-right:50px; margin-left:50px",
#                  
#                  fluidRow(style='padding:10px; font-size: 5rem; 
#                                                           font-weight: 900; color:#3e6d72',
#                           textOutput("ihe_fhiscore")),
#                  fluidRow(style='padding:10px; font-size: 2.5rem; color:black',
#                           "range: -0.2 – 0.2")
#           ),
#           column(5,style="margin-right:50px; margin-left:50px",
#                  
#                  fluidRow(style='padding:10px; font-size: 5rem; 
#                                                           font-weight: 900; color:#3e6d72',
#                           textOutput("ihe_percentile")),
#                  fluidRow(style='padding:10px; font-size: 2.5rem; color:black',
#                           htmlOutput("ihe_percentile_info"))
#           )
#         ),
#         br(),
#         fluidRow(style='background-color: #e9b73f',
#                  align="left",
#                  div(class="lvl2_title_wh",
#                      "School Demographics Breakdown")),
#         br(),
#)
          
#if(is.null(input$sector) & is.null(input$state)) {
#  updateSelectizeInput(session,
#                       inputId = "dname",
#                       choices = fsums %>%
#                         distinct(dname) %>%
#                         arrange(dname) %>% 
#                         pull(dname),
#                       selected =NULL,
#                       options = list(placeholder = 'Select institutions',
#                                      onInitialize = I('function() { this.setValue(""); }')),
#                       server=TRUE)
#}

#if(is.null(input$state) & !is.null(input$sector)) {
#  updateSelectizeInput(
#    inputId = "dname",
#    choices = fsums %>%
#      filter(sector %in% input$sector) %>%
#      distinct(dname) %>%
#      arrange(dname) %>% 
#      pull(dname),
#    options = list(placeholder = 'Select institutions',
#                   onInitialize = I('function() { this.setValue(""); }')),
#    server=TRUE)
#}

#if(is.null(input$sector)) {
#  updateSelectizeInput(
#    inputId = "dname",
#    choices = fsums %>%
#      filter(sector %in% input$sector) %>%
#      distinct(dname) %>%
#      arrange(dname) %>% 
#      pull(dname),
#    options = list(placeholder = 'Select institutions',
#                   onInitialize = I('function() { this.setValue(""); }')),
#    server=TRUE)
#}

#if(!is.null(input$state) & !is.null(input$sector)) {
#  
#  updateSelectizeInput(
#    inputId = "dname",
#    choices = fsums %>%
#      filter(state %in% input$state) %>%
#      filter(sector %in% input$sector) %>%
#      distinct(dname) %>%
#      arrange(dname) %>% 
#      pull(dname),
#    options = list(placeholder = 'Select institutions',
#                   onInitialize = I('function() { this.setValue(""); }')),
#    server=TRUE)
#}


# Resources
#-----1. Conditional panel with action button
# https://stackoverflow.com/questions/39405136/how-to-use-actionbutton-as-a-trigger-to-conditionalpanel
# https://campus.datacamp.com/courses/case-studies-building-web-applications-with-shiny-in-r/create-your-own-word-cloud-in-shiny?ex=12
#-----2. Show/Hide button
#https://stackoverflow.com/questions/44790028/show-hide-entire-box-element-in-r-shiny/44790978
#-----3. Highcharts Bar charts
#https://www.highcharts.com/blog/tutorials/bar-chart-for-categorical-data/