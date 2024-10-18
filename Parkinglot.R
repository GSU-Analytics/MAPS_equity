
### Feature Row ------------------------------------------------------

output$subscore_title <- renderText({
  if (is.null(input$subscorePlot_click$y)) return(HTML("<font size = 5 color='red'><center>Click a score above to view details.</center></font>"))
  else {
    HTML("<font size=5><center>Details for", current_nicename(), "</font></center>")
  }
})

current_feature <- eventReactive(input$subscorePlot_click, {
  
  req(current_nicename())
  
  
  
  cf <- features %>%
    filter(nice_name == current_nicename()) %>% 
    filter(control_of_institution == (fsums %>% 
                                        filter(dname == input$dname) %>% 
                                        distinct(control_of_institution) %>% 
                                        pull(control_of_institution)))
  
  cat(file=stderr(), "532 plot click current feature yields variables:", cf %>% select(variable) %>% unlist(), "based on current_nicename:", current_nicename(), "\n")
  
  
  return(cf)
})

raw_features_data <- eventReactive(input$subscorePlot_click, {
  
  req(current_feature())
  
  variables <- current_feature() %>% 
    distinct(variable) %>% 
    pull(variable)
  
  rfd <- raw_values %>% 
    filter(dname == input$dname) %>% 
    filter(variable %in% variables) %>% 
    left_join(current_feature() %>% select(variable, fraction_part, sequence, var_title)) %>% 
    arrange(year, desc(fraction_part), sequence) %>% 
    deselect(fraction_part, sequence)
  
  cat(file=stderr(), "557 raw_features_data pulled based on current_feature variables:", current_feature() %>% select(variable) %>% unlist(), "\n")
  
  return(rfd)
})

raw_feature_slope_plots <- eventReactive(input$subscorePlot_click, {
  req(raw_features_data())
  req(current_nicename())
  
  
  rf_graphs <- raw_features_data() %>% 
    group_by(variable) %>% 
    nest() %>% 
    mutate(slope_graphs = map(data, raw_feature_slope_plot)) %>% 
    #arrange(variable) %>% 
    pull(slope_graphs)
  
  cat(file=stderr(), "574 plots generated based on raw_features_data from current_nicename:", current_nicename(), "\n")
  
  return(rf_graphs)
  
})

observeEvent(input$subscorePlot_click, {
  
  cat(file=stderr(), "observe plot click", input$subscorePlot_click$y, "create plot outputs based on current_nicename:", current_nicename(), "\n")
  
  
  req(raw_feature_slope_plots())
  
  iwalk(raw_feature_slope_plots(), ~{
    output_name <- paste0("raw_feature_slope_plot_", .y)
    output[[output_name]] <- renderPlot(.x)
  })
})

output$feature_row <- renderUI({
  req(current_feature())
  req(current_nicename())
  
  if(is.null(current_nicename())) return()
  
  
  mylist <- current_feature() %>% 
    arrange(desc(fraction_part), sequence) %>% 
    deselect(sequence) %>% 
    distinct() %>% 
    rowwise() %>%
    group_split() %>%
    imap(~{
      tagList(fluidRow(
        column(2, HTML(paste0("<b>", .x$var_title, "</b>"))),
        column(4, .x$description),
        column(4, 
               plotOutput(
                 outputId = paste0("raw_feature_slope_plot_", .y)
               )
        )
      ),
      br()
      )
    })
  
  equation_text <- paste0("<b><center>", current_nicename(), text_describe_feature(current_feature()), "</b></center>")
  derivation_text  <- tagList(fluidRow(HTML(equation_text)), br())
  
  table_header <- tagList(fluidRow(
    column(2, HTML("<u>Variable</u>")),
    column(4, HTML("<u>IPEDS Definition</u>")),
    column(4, HTML("<u>Values over Time</u>")))
  )
  
  return(tagList(derivation_text, table_header, mylist))
  
})



### Derived Plots -----------------------------------------------------------

output$derived_value_over_time_plot <-  renderPlot({
  if(is.null(current_name())) return()
  
  req(current_name())
  
  cat(file=stderr(), "derived value plot current name:", current_name(),"\n")
  cat(file=stderr(), "derived value plot ihe:", input$dname,"\n")
  
  p <- long_ft %>% 
    filter(dname == input$dname) %>% 
    filter(name_fsums == current_name()) %>% 
    deselect(institution_entity_name) %>% 
    mutate(nice_name = str_wrap(nice_name, width = 30)) %>% 
    ggplot(aes(x = year, y = value)) +
    geom_line(color = "grey50", lty = 2) +
    geom_point(color = "grey50") +
    geom_smooth(aes(color = category), method = "lm", se = F) +
    labs(title = "Derived Values over Time", x = "Year", y = "Value") +
    guides(color = "none") +
    ylab(NULL)
  #ylab(current_name())
  
  if(str_detect(current_name(), "pct|ratio")) {
    p + scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 0.1))
  } else if(str_detect(current_name(), "enrollment|retention")) {
    p + scale_y_continuous(labels = scale_si_unit())
  } else {
    p + scale_y_continuous(labels = si_scale_big_dollar)
  }
  
})


output$ft_subscore_distPlot <- renderPlot({
  if(is.null(current_name())) return()
  
  long_ft %>% 
    filter(name_fsums == current_name()) %>% 
    mutate(nice_name = str_wrap(nice_name, 20)) %>%
    ggplot(aes(x = value)) +
    geom_vline(data = long_ft %>% 
                 filter(dname == input$dname) %>% 
                 filter(name_fsums == current_name()) %>% 
                 mutate(nice_name = str_wrap(nice_name, 20)) %>% 
                 mutate(Year = factor(year)),
               aes(xintercept = value)) +
    #scale_color_viridis_d(direction = -1) +
    #guides(color = "none") +
    #new_scale_color() +
    geom_density(aes(x = value, color = category)) +
    #guides(color = F) + 
    guides(color = "none") +
    # geom_label(data = fsums %>% 
    #              mutate(nice_name = str_wrap(nice_name, 20)) %>% 
    #              filter(unitid == 222822), 
    #            aes(label = institution_entity_name, x = weighted_value, y = 0)) +
    labs(x = "Raw Value", y = "Density") +
    
    #guides(color = F) +
    
    ggtitle("Raw Value Location in Distribution")
})


### Subscore Distributions Plot ---------------------------------------------


output$fsums_subscore_distPlot <- renderPlot({
  if(is.null(current_name())) return()
  fsums %>% 
    filter(name == current_name()) %>% 
    mutate(nice_name = str_wrap(nice_name, 20)) %>%
    ggplot(aes(x = weighted_value, color = category)) + #Need to filter sector for these if not showing separately.
    geom_density() +
    geom_vline(data = fsums %>% 
                 filter(dname == input$dname) %>% 
                 filter(name == current_name()) %>% 
                 mutate(nice_name = str_wrap(nice_name, 20)), 
               aes(xintercept = weighted_value), color = "black") +
    # geom_label(data = fsums %>% 
    #              mutate(nice_name = str_wrap(nice_name, 20)) %>% 
    #              filter(unitid == 222822), 
    #            aes(label = institution_entity_name, x = weighted_value, y = 0)) +
    labs(x = "Weighted Score", y = "Density") +
    guides(color = "none") +
    ggtitle("Weighted Score Location in Distribution")
})


require(dplyr)
require(purrr)
require(tidyr)
require(gapminder)
#> Loading required package: gapminder
data(gapminder, package = "gapminder")

gp <- gapminder %>%
  arrange(desc(year)) %>%
  distinct(country, .keep_all = TRUE)

gp2 <- gapminder %>%
  nest(-country) %>%
  mutate(
    data1 = map(data, mutate_mapping, hcaes(x = lifeExp, y = gdpPercap), drop = TRUE),
    data1 = map(data1, list_parse)
  ) %>%
  mutate(
    data2 = map(data, mutate_mapping, hcaes(x = year, y = gdpPercap), drop = TRUE),
    data2 = map(data2, list_parse)
  ) %>%
  select(-data)
#> Warning: All elements of `...` must be named.
#> Did you want `data = c(continent, year, lifeExp, pop, gdpPercap)`?

gptot <- left_join(gp, gp2)
#> Joining, by = "country"

hc <- hchart(
  gptot,
  "point",
  hcaes(
    lifeExp,
    gdpPercap,
    name = country,
    size = pop,
    group = continent
  )
) %>%
  hc_yAxis(type = "logarithmic")

hc %>%
  hc_tooltip(useHTML = TRUE, pointFormatter = tooltip_chart(accesor = "data1")) %>%
  hc_tooltip(useHTML = TRUE, pointFormatter = tooltip_chart(accesor = "data2"))


library("shiny")
library("highcharter")

ui <- shinyUI(
  fluidPage(
    column(width = 8, highchartOutput("hcontainer", height = "500px")),
    column(width = 4, highchartOutput("plot"))
  )
)

server <- function(input, output) {      
  
#  a <- data.frame(b = LETTERS[1:10], c = 11:20, d = 21:30, e = 31:40)

  
  output$hcontainer <- renderHighchart({      
    
    canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.category]);}")
    legendClickFunction <- JS("function(event) {Shiny.onInputChange('legendClicked', this.name);}")
    
#    highchart() %>%
#      hc_xAxis(type = "category") %>%
#      hc_legend(enabled = TRUE) %>%
    
    drilldown_df %>%
      select(dname,category.x,score_per_inst_category) %>%
      mutate(drilldown=tolower(category.x)) %>%
      rename("name"=category.x, "y"=score_per_inst_category) %>%
      hchart("column",
           hcaes(x=name, y=y, group=name), 
           animation=T, 
           allowPointSelect=T,
           connectorWidth=8,
           marker=list(radius=10)) %>%
      hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE),
          stacking = FALSE, 
          events = list(click = canvasClickFunction, legendItemClick = legendClickFunction)
        )
      )

    
  })      
  
  makeReactiveBinding("outputPlot")
  
  observeEvent(input$canvasClicked, {
    #outputText <<- paste0("You clicked on series ", input$canvasClicked[1], " and the bar you clicked was from category ", input$canvasClicked[2], ".") 
    outputPlot<<-drilldown_df %>% ungroup() %>%
      filter(category.x=="Assets") %>%
      select(data) %>%
      unnest(data) %>%
      hchart("line",
             hcaes(x=name, y=value), 
             animation=T, 
             allowPointSelect=T,
             connectorWidth=8,
             marker=list(radius=10))
    
#    outputPlot<<-highchart() %>%
#      hc_xAxis(type = "category") %>%
#      hc_legend(enabled = TRUE) %>%
#      hc_add_series(
#        data = drilldown_df %>% ungroup() %>%
#          filter(category.x=="Assets") %>%
#          select(data) %>%
#          unnest(data),
#        type = "line",
#        hcaes(name = name, y = value),
#        name = "Category",
#        colorByPoint = TRUE,
#        connectorWidth=5,
#        marker=list(radius=5)
#      )
    
    })
  
#  observeEvent(input$legendClicked, {
#    outputText <<- paste0("You clicked into the legend and selected series ", input$legendClicked, ".")
#  })
  
  output$plot <- renderHighchart({ 
    
    outputPlot
    
  })
    
    
#  output$text <- renderText({
#    outputText      
#  })
}

shinyApp(ui, server) 
  
