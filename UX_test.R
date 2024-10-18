library(shiny)
library(tidyverse)
library(waiter)
library(shinycssloaders)

df<-data.frame(c1=c("a","a","a","a","a","b","b","b","b","b"), c2=c(1,1,1,1,2,2,2,2,3,3), c3=c(4,4,4,4,5,5,6,6,7,7))


ui <- fluidPage(


  #selectizeInput(inputId='select1',
  #               label='Select something',
  #               multiple = FALSE,
  #               selected = "none",
  #               choices=c("none",unique(df$c1))
  #),
  uiOutput("select1"), #%>% withSpinner(color="#0dc5c1"),
  
  uiOutput("select2"), #%>% withSpinner(color="#0dc5c1"),
  
  uiOutput("select3"), #%>% withSpinner(color="#0dc5c1")
  
  
                 
)

server <- function(input, output, session){

  output$select1 <- renderUI({

    radioGroupButtons(
      inputId = "select1",
      label= "Select1",
      status = "myClass",
      choices = unique(df$c1),
      size = "lg",
      width = "100%",
      direction = "horizontal",
      justified = F,
      individual = T,
      selected=F)
  })
  
  output$select2 <- renderUI({

    selectizeInput(inputId='select2',
                   label='Select something',
                   multiple = FALSE,
                   selected = "none",
                   choices=c("none",unique(df$c2))
    )
  })

  output$select3 <- renderUI({

    selectizeInput(
    inputId='select3',
                 label='Select something',
                 selected = "none",
                 choices= c("none",unique(df$c3)))
  })
    
  
  observeEvent({
    input$select1 
    input$select2}, {
      
      if(input$select2=="none") {
        updateSelectizeInput(
          session,
          inputId = "select3",
          choices = c("none",{df %>%
            filter(c1 %in% input$select1) %>%
            distinct(c3) %>%
            pull(c3)})
        )
      }
      
      if(is.null(input$select1) & input$select2!="none") {
        updateSelectizeInput(
          inputId = "select3",
          choices = c("none",{df %>%
            filter(c2 %in% input$select2) %>%
            distinct(c3) %>%
            pull(c3)}),
          server = TRUE
          )
      }
      
      if(!is.null(input$select1) & input$select2!="none") {
        
        updateSelectizeInput(
          inputId = "select3",
          choices = c("none",{df %>%
            filter(c1 %in% input$select1) %>%
            filter(c2 %in% input$select2) %>%
            distinct(c3) %>%
            pull(c3)}),
          server = TRUE
          )
      }
      
      if(is.null(input$select1) & input$select2=="none") {
        
        updateSelectizeInput(
          inputId = "select3",
          choices = c("none",{df %>%
            distinct(c3) %>%
            pull(c3)}),
            
          selected="none",
          server=TRUE)
      }
    })


}

shinyApp(ui=ui, server=server)


server <- function(input, output) {
    
    withProgress(message = 'Making plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      for (i in 1:n) {
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
      })
}

ui <- shinyUI(basicPage(

))

shinyApp(ui = ui, server = server)



# This function computes a new data set. It can optionally take a function,
# updateProgress, which will be called as each row of data is added.
compute_data <- function(updateProgress = NULL) {
  # Create 0-row data frame which will be used to store data
  dat <- data.frame(x = numeric(0), y = numeric(0))
  
  for (i in 1:10) {
    Sys.sleep(0.25)
    
    # Compute new row of data
    new_row <- data.frame(x = rnorm(1), y = rnorm(1))
    
    # If we were passed a progress update function, call it
    if (is.function(updateProgress)) {
      text <- paste0("x:", round(new_row$x, 2), " y:", round(new_row$y, 2))
      updateProgress(detail = text)
    }
    
    # Add the new row of data
    dat <- rbind(dat, new_row)
  }
  
  dat
}



data(mpg, package = "ggplot2")

mpgg <- mpg %>% 
  filter(!manufacturer %in% c("volkswagen", "chevrolet")) %>% 
  filter(class %in% c("compact", "midsize", "subcompact")) %>% 
  group_by(class, manufacturer) %>% 
  summarize(count = n()) %>% 
  ungroup()

categories_grouped <- mpgg %>%
  select(class, manufacturer) %>%
  group_by(name = class) %>% 
  summarise(categories = list(manufacturer)) %>% 
  list_parse()

hchart(
  mpgg,
  "column", 
  name = "Cars",
  hcaes(y = count)
) %>% 
  hc_xAxis(
    # specify the grouped categories
    categories = categories_grouped, 
    # styling a little bit
    labels = list(style = list(fontSize = "10px"))
  ) %>%
  hc_add_dependency("plugins/grouped-categories.js")





