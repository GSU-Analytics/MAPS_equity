# Author: Jonathan Zadra
# Created: Fri Jul 16 12:27:01 2021
# Description: Draft dashboard for financial metrics
# test

# DONE: Add definitions of variables from IPEDS.
# DONE: Sort variables by category and then alphabetical
# DONE: Add institutional profile info to other tab
# DONE: Clean up subscore names
# DONE: Move distribution plot above subscore plot
# DONE: Add comparison selection
# TODO: Add checkboxes to show additional info and details
# TODO: Add checkboxes to show all in state or all in region
# TODO: Add ability to compare to HBCUs or Tribal Colleges
# TODO: Add score and percentile to the distribution plot
# TODO: Add option to switch to table for the subscores
# TODO: For compare, allow to select categories (sector, carnegie for density/summary)
# TODO for subscore selection, include definitions and classification rules to show how the score ended up the way it did.
# TODO: Also add where it sits within the state and sector, in addition to nationally per Harrison Keller
# TODO: Add some text on the subscore saying whether it is good or bad, direction of change (Getting better or worse?), and also how it's weighted (how much impact does it have)
# TODO: Add disclaimer about not using this for evil.
# TODO: Profile tab - add some college scorecard data
# TODO: Per dale, switch order of subscore plot and distribution plot, then follow with details.
# DONE Why is University of Florida scores on the index plot different than on the compare plot? Scores are changing when other schools are added (done: it was just cutting off due to limits)
# TODO: Add plot doubleclick to adjust subscore and see new index score?
# TODO: Add ability to share current view (or bookmark it!)
# TODO: Keep color of compare schools constant as new ones added
# TODO: add info on which raw feature is the biggest contributor to the derived score when multiple vars in numerator
# TODO: Text that tells them the top 3 things they could change to improve the score
# TODO: Revenue, Expenses, Assets, Debt, Student in one polot, or 3 plots grouped by
# TODO: Update for 2020 and check hardcoded 2019 filters.  Add windowed thing to see historical scores?  Keep in mind things like weighting of raw cutoffs for early vs late years will need to be dynamic.
# Revenues and expenses - Income Statement
# Assets and Debt - Balanced Sheet
# Student - nonfinancial indicators


# DONE: Check whether the total score distribution is mean or sum (mean should only go +/- 2): is sum.


# For the models
# TODO: decide on whether to exclude grad only
# DONE: carnegie classification (decided not to use)
# TODO: figure out how subscores are correlated wtih main score to see if certain ones lots of institutions struggle with, etc.
# TODO: WTF IS THIS? long_ft %>% filter(str_detect(name, "approp")) %>% ggplot(aes(x = value)) + geom_density()
# TODO: Graduated weights by year for absolute cutoffs.

source("global.R")

# Run the application

shinyApp(ui = ui, server = server, enableBookmarking = "url")



# Parking lot

## Big Picture Insights Tab

# output$interpretation1 <- renderUI({
#  HTML("<br><p style='font-family: 'Roboto Mono', sans-serif;
#      font-size: 16px;'><b>1. REVENUE:</b></p></h4>
#      <p style='text-indent: 40px;line-height:1.3em;font-size: 16px'><u>1.1. Net Income</u></p>
#      <p style='margin-left:10%;line-height:1.3em;font-size: 16px'>Net income is revenue minus expenses, interest, and tax. Net income is helpful in understanding how much revenue does or does not exceed expenses.</p>
#
#      <p style='text-indent: 40px;line-height:1.3em;font-size: 16px'><u>1.2. Revenue sources as percent of total</u></p>
#      <p style='margin-left:10%;line-height:1.3em;font-size: 16px'>Revenue sources as percent of total is a calculated metric that assesses income streams. If any revenue source is greater than 50% (or 75% for tuition and fees), it is an indicator that the institution’s income may not be sufficiently diversified (basically, don’t put all your eggs in one basket). This is a risk because if any one source of revenue decreases substantially, the institution may not be able to cover expenses. The benefit of having a diversified revenue stream (multiple sources of income) is financial resiliency in the event that one or two revenue sources drop or evaporate.</p>")
# })
#### Topic1. Scatter Plot

# efa_fsums_react1<-reactive({
#  efa_fsums_total %>%
#    select(unitid, ihe_name, institution_entity_name, city, state, sector,
#           total, pell, pell_pct_per_inst, grand_completion_rate_per_inst,
#           enrollment_total, tuition_in_state_avg_per_inst) %>%
#    mutate(enrollment_total=log(enrollment_total)) %>%
#    rename('Pell Ratio' = pell_pct_per_inst,
#           'Completion Rate' = grand_completion_rate_per_inst,
#           'Enrollment Size' = enrollment_total,
#           'Tuition' = tuition_in_state_avg_per_inst) %>%
#    filter(pell=="pell_count_sum_per_inst")
# })

# efa_fsums_react1_1<-reactive({
#  efa_fsums_total %>%
#    select(unitid, ihe_name, institution_entity_name, city, state, sector,
#           total, pell, pell_pct_per_inst, grand_completion_rate_per_inst,
#           enrollment_total, tuition_in_state_avg_per_inst) %>%
#    mutate(enrollment_total=log(enrollment_total)) %>%
#    filter(pell=="pell_count_sum_per_inst")
# })

# output$bigpic1<-renderPlotly({

#  Fig<-function(topics,sectors){
#    a1<-efa_fsums_react1() %>% filter(sector==sectors) %>%
#      distinct() %>%
#      drop_na(topics)
#
#    fv<-lm(reformulate('total',topics),
#           data = a1) %>%
#      fitted.values()
#
#    plot_ly(a1, x = ~total, y = ~get(topics), hoverinfo='text',
#            text = glue::glue("University: <b>{a1$institution_entity_name}</b>
#                                     Location: <b>{a1$city},{a1$state}</b>
#                                     FHI: <b>{round(a1$total,4)}</b>"),
#            mode="markers", name=sectors) %>%
#      add_markers(y = ~get(topics)) %>%
#      add_trace(data = a1, x = ~total, y = fv, mode = "lines") %>%
#      layout(xaxis = list(title = 'Financial Health Index'),
#             yaxis = list(zeroline=T, title = topics), legend = NULL)
#    #add_lines(x = ~a1$total, y = fitted(fit))
#  }

#  fig_1<-Fig(input$Topics, 'Public, 4-year or above')
#  fig_2<-Fig(input$Topics, 'Public, 2-year')
#  fig_3<-Fig(input$Topics, 'Private not-for-profit, 4-year or above')
#  fig_4<-Fig(input$Topics, 'Private not-for-profit, 2-year')
#  fig_5<-Fig(input$Topics, 'Private for-profit, 4-year or above')
#  fig_6<-Fig(input$Topics, 'Private for-profit, 2-year')
#
#  fig <- subplot(fig_1, fig_2, fig_3, fig_4, fig_5, fig_6,nrows = 2)
# })

# output$bigpic1<-renderPlotly({
# efa_fsums_react1() %>%
#  ggplot(aes(x=total, y=as.numeric(get(input$Topics)), color=sector)) +
# scale_color_brewer(palette="RdBu") +
# geom_point(aes(text = glue::glue("University: <b>{institution_entity_name}</b>
#                              Location: <b>{city},{state}</b>
#                             FHI: <b>{round(total,4)}</b>
#                            {input$Topics}: <b>{round(get(input$Topics),4)}</b>")),alpha=0.4, size=0.5)+
# geom_smooth(size=2,method = "lm", se = T) +
# geom_smooth(size=1,alpha=0.8,method = "loess", se = F) +
# scale_x_binned(n.breaks=10) +
# theme(legend.position = "bottom",
#      axis.text.x =element_text(angle = 0, vjust = 0.8, hjust=0.8, size=7)) +
# theme_minimal()+
# labs(title=glue::glue("Comparing {input$Topics} and Financial Health Score of Institutions"),
#     x ="Financial Health Score",
#     y = glue::glue("{input$Topics}"))+
# facet_wrap( ~ sector, nrow = 2)
# facet_grid(cols=vars(sector),nrow=2)

# ggplotly(gg1,tooltip="text")
# })




## Index Tab ----
#                  tabPanel(h2(HTML("<B>INDEX SCORES</B>")),
#                           br(),
#                           ### Select School ----
#                            mainPanel(
#                              fluidRow(
#                             column(3,
#                                    selectizeInput(
#                                      inputId = "sector",
#                                      label = "Sector",
#                                      choices = fsums %>%
#                                        distinct(sector) %>%
#                                        arrange(sector) %>%
#                                        pull(sector),
#                                      multiple = FALSE,
#                                      options = list(placeholder = 'Select Sector',
#                                                     onInitialize = I('function() { this.setValue(""); }'))
#                                    )),
#                             column(2,
#                                    selectizeInput(
#                                      inputId = "state",
#                                      label = "State",
#                                      choices = sort(as.character(unique(fsums$state))),
#                                      multiple = FALSE,
#                                      options = list(placeholder = 'Select State',
#                                                     onInitialize = I('function() { this.setValue(""); }'))
#                                    )
#                             ),
#                             column(5,
#                                    selectizeInput(
#                                      inputId = "dname",
#                                      label = "Select University",
#                                      choices = sort(as.character(unique(fsums$dname))),
#                                      multiple = FALSE,
#                                      options = list(placeholder = 'Select University',
#                                                     onInitialize = I('function() { this.setValue(""); }'))
#                                    )
#                             ),
#                             column(1,
#                                    br(),
#                                    bookmarkButton(id = "bookmark",
#                                                   label = "Share...",
#                                                   icon = shiny::icon("share", lib = "font-awesome"))
#                             )),
#                           fluidRow(
#                             ### Profile ----
#                             tabsetPanel(type = "pills", id = "index_tab",
#                                         tabPanel(h2(HTML("Profile")),
#                                                  fluidRow(
#                                                    htmlOutput("ihe_name")
#                                                  ),
#                                                  br(),
#                                                  fluidRow(
#                                                    column(4,
#                                                           htmlOutput("ihe_location")),
#                                                    column(4,
#                                                           htmlOutput("ihe_enrollment")),
#                                                    column(4,
#                                                           htmlOutput("ihe_graduation")),
#                                                    column(4,
#                                                           htmlOutput("ihe_tuition"))
#
#                                                  )),
#                                         ###  Index ----
#                                         tabPanel(h2(HTML("Financial Index")),
#                                                  br(),
#                                                  br(),
#                                                  fluidRow(
#                                                    plotOutput("distlocationPlot", width = 1600, height = 300)
#                                                  ),
#                                                  br(),
#                                                  fluidRow(
#                                                    plotOutput("subscorePlot", width = 1600, height = 600, click = "subscorePlot_click")
#                                                  ),
#                                                  br(),
#                                                  htmlOutput("subscore_title"),
#                                                  br(),
#                                                  uiOutput("feature_row"),
#                                                  br(),
#
#                                                  fluidRow(
#                                                    # column(4,
#                                                    #        plotOutput("ft_subscore_distPlot")),
#                                                    column(12,
#                                                           plotOutput("derived_value_over_time_plot"))#,
#                                                    # column(4,
#                                                    #        plotOutput("fsums_subscore_distPlot"))
#                                                  )
#                                         ),
#                                         ### Compare ----
#                                         tabPanel(h2(HTML("Compare")),
#                                                  fluidRow(
#                                                    column(4,
#                                                           selectizeInput(
#                                                             inputId = "compare_state",
#                                                             label = "State (optional filter)",
#                                                             choices = sort(as.character(unique(fsums$state))),
#                                                             selected = NULL,
#                                                             multiple = TRUE,
#                                                             options = list(placeholder = 'Select State (optional)')
#                                                           )
#                                                    ),
#                                                    column(4,
#                                                           selectizeInput(
#                                                             inputId = "compare_dname",
#                                                             label = "Select Comparison Universities",
#                                                             choices = NULL,
#                                                             multiple = TRUE,
#                                                             selected = NULL,
#                                                             options = list(placeholder = 'Select up to 5')
#                                                           )
#                                                    ),
#                                                  ),
#                                                  br(),
#                                                  fluidRow(plotOutput("compare_distlocationPlot")),
#                                                  fluidRow(plotOutput("compare_subscorePlot")))
#                             )
#                           )
#                  ),
#                  tabPanel(h2(HTML("<B>BIG PICTURE INSIGHTS</B>")),
#                           br(),
#                           br(),
#                           fluidRow(
#                             selectInput("Topics",
#                                        label = "Select Topic",
#                                        choices = c('Pell Ratio',
#                                                    'Completion Rate',
#                                                    'Enrollment Size',
#                                                    'Tuition')
#                             )
#                           ),
#                           fluidRow(
#                             plotlyOutput("bigpic1", width = 1200, height = 800),
#                             bootstrapPage(
#                               absolutePanel(
#                                 id = "controls", class = "panel panel-default", fixed = FALSE,
#                                 draggable = TRUE, top = 200, left = 300, right = 200, bottom = "auto",
#                                 width = 400, height = "auto", style="opacity: .80",
#                                 HTML('<button data-toggle="collapse" data-target="#demo">Interpretation</button>'),
#                                 tags$div(id = 'demo',  class="collapse",
#                                          htmlOutput("interpretation1")))
#                             )
#                           ),
#                           br(),
#                           fluidRow(
#                             column(4,htmlOutput("sector_selector",width = 300)),#add selectinput boxs
#                             #column(4,htmlOutput("state_selector",width = 160))
#                           ),
#                           fluidRow(
#                             plotOutput("bigpic2",width= 1200, height = 1000,
#                                          hover = hoverOpts(
#                                            id = "plot_hover"))),
#                           br()
#                           ),
#
#
#                  tabPanel(h2(HTML("<B>FROM THE FIELD</B>")),
#                           htmlOutput("FTF")),
#                  tabPanel(h2(HTML("<B>PLAIN LANGUAGE GLOSSARY</B>")),
#                           htmlOutput("glossary")),
#                  tabPanel(h2(HTML("<B>METHODOLOGY</B>")),
#                           htmlOutput("methodology")),
#                  tabPanel(h2(HTML("<B>ACKNOWLEDGEMENTS</B>")),
#                           htmlOutput("acknowledgements"))
#      )
#    )
#  )
# }


### Topic2. Distribution Plot

# -----. Interactive and reactive UI control at the server level

# output$sector_selector = renderUI({ #creates Sector select box object called in ui
#  selectInput(inputId = "sector1", #name of input
#              label = "Sector:", #label displayed in ui
#              choices = as.character(unique(efa_fsums_total$sector)),
#              # calls unique values from the State column in the previously created table
#              selected = "Public, 4-year or above",
#              multiple = FALSE) #default choice (not required)
# })

# output$bigpic2<-renderPlot({
#
#  efa_fsums_total %>%
#    select(unitid, sector, state, total, pell) %>%
#    filter(pell=="pell_count_sum_per_inst") %>%
#    distinct() %>%
#    drop_na(state) %>%
#    filter(sector==input$sector1) %>%
#    #filter(state=="Delaware") %>%
#    ggplot(aes(x=total, y=fct_reorder(state, total, .fun = sum), fill=stat(x))) +
#    geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
#    scale_fill_viridis_c(name = "FHI", option = "C") +
#    labs(title=glue::glue("Comparing Distribution of Financial Health Index Across States: "),
#         x ="Financial Health Score",
#         y = glue::glue("Density (by state)"))
# })

# output$hover_info <- renderPrint({
#  hover <- input$plot_hover
#  y <- nearPoints(efa_fsums_total %>%
#                    select(unitid, sector, state, total, pell) %>%
#                    filter(pell=="pell_count_sum_per_inst") %>%
#                    distinct() %>%
#                    drop_na(state) %>%
#                    filter(sector==input$sector1), input$plot_hover)[input$state]
#  req(nrow(y) != 0)
#  y
# })
#
# options(shiny.reactlog = TRUE)

# setBookmarkExclude("bookmark")

# observeEvent(input$bookmark, {
#  cat(file=stderr(), "257 bookmark event", "\n")
#  session$doBookmark
# })

# current_name <- reactiveVal(NULL)
# current_nicename <- reactiveVal(NULL)

# observeEvent(input$subscorePlot_click, {
#  if (is.null(input$subscorePlot_click$y)) return("")

#  cat(file=stderr(), "==========================================\nNEW PLOT CLICK SECTION\n==========================================\n")

#  lvls_name <- levels(fsums %>%
#                        filter(dname == input$dname) %>%
#                        arrange(category, name) %>%
#                        mutate(name = forcats::as_factor(name),
#                               name = fct_reorder(name, as.numeric(as.factor(category))) %>%
#                                 fct_rev()) %>%
#                        pull(name))

#  new_name <- lvls_name[round(input$subscorePlot_click$y)]
#  current_name(new_name)

#  lvls_nicename <- levels(fsums %>%
#                            filter(dname == input$dname) %>%
#                            arrange(category, name) %>%
#                            mutate(nice_name = forcats::as_factor(nice_name),
#                                   nice_name = fct_reorder(nice_name, as.numeric(as.factor(category))) %>%
#                                     fct_rev()) %>%
#                            pull(nice_name))
#
#  new_nicename <- lvls_nicename[round(input$subscorePlot_click$y)]

#  current_nicename(new_nicename)

#  cat(file=stderr(), "289 observe click current_nicename", current_nicename(), "\n")
#  cat(file=stderr(), "290 observe click current_name", current_name(),"\n")
# })

# observeEvent(input$dname, {
#  current_name(NULL)
#  current_nicename(NULL)
# })
