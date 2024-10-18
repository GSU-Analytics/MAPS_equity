
  sector_choice<-fsums %>%
    filter(dname=="Amridge University (100690)") %>%
    distinct(sector) %>%
    pull(sector) %>% 
    unique



  
  ds<-density(fsums %>%
                distinct(sector, total, dname,pct_rank,unitid) %>%
                filter(sector %in% sector_choice) %>%
                pull(total))
  
  ds_react<-data.frame(x=ds$x, y=ds$y)
  
  
  total<-fsums %>% 
    distinct(sector, total, dname, pct_rank, unitid) %>%
    filter(dname %in% c("Amridge University (100690)",
                        "Bethune-Cookman University (132602)",
                        "Rollins College (136950)")) %>%
    pull(total)
#  ,"Bethune-Cookman University (132602)",
#  "Rollins College (136950)"
  
  coordinate_react<-ds_react %>%
    filter(x>total & x<(total+0.02)) %>% 
    slice(which.max(x)) %>%
    round(.,5)
  
  coordinate_react<-sapply(total, function(i) {ds_react %>%
      filter(x>i & x<(i+0.02)) %>% 
      slice(which.max(x)) %>%
      round(.,5)}) %>%
    t(.) %>%
    as.data.frame %>%
    rownames_to_column() %>%
    mutate(coloract = ifelse(rowname=='1', "#e9b73f", "#7e95ac"))%>%
    mutate(cluster = ifelse(rowname=='1', "prime", "others"))



  distplot_react<-hchart(ds_react,
         type = "areaspline", 
         color = "#bfdcd7", 
         lineColor = c("#3e6d72","#24aa90"),
         name = sector_choice,
         opacity = 0.2,
         selected=T) %>%
    hc_size(width=1000, height=300)
  
  base.plot<-distplot_react
  
  ifelse(input$dname=="", return(base.plot),
         return(
           
           base.plot %>%
             hc_add_series(data = coordinate_react, 
                           name= "School",
                           type = "column", 
                           hcaes(x,y,group="rowname",
                                 color = coloract),
                           pointWidth = 5) %>%
             hc_tooltip(crosshairs = TRUE,
                        pointFormat= "<b>FHI Score:</b> {point.x} <br>
                 <b>Density:</b> {point.y} <br>")
         )
  )
  
})
  
  ui <- fluidPage(
    tags$h1("radioGroupButtons examples"),
    
    radioGroupButtons(
      inputId = "somevalue1",
      label = "Make a choice: ",
      choices = c("A", "B", "C"),
      selected=F
    ),
    verbatimTextOutput("value1"),
    
    radioGroupButtons(
      inputId = "somevalue2",
      label = "With custom status:",
      choices = names(iris),
      status = "primary",
      selected=F
    ),
    verbatimTextOutput("value2"),
    
    radioGroupButtons(
      inputId = "somevalue3",
      label = "With icons:",
      choices = names(mtcars),
      selected=F,
      checkIcon = list(
        yes = icon("check-square"),
        no = icon("square-o")
      )
    ),
    verbatimTextOutput("value3")
  )
  server <- function(input, output) {
    
    output$value1 <- renderPrint({ input$somevalue1 })
    output$value2 <- renderPrint({ input$somevalue2 })
    output$value3 <- renderPrint({ input$somevalue3 })
    
  }
  shinyApp(ui, server)
  