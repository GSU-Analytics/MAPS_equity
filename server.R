
#source("global.R")
#source("C:/Users/allso/Documents/MAPS_equity_dash/MAPS_equity_dash/global.R")



# define server -------------------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  
  output$sector <- renderUI({
    radioGroupButtons(
      inputId = "sector",
      label= "Sector",
      status = "myClass",
      choices = c("Private, 2-year","Private, 4-year or above","Proprietary, 2-year",
                  "Proprietary, 4-year or above","Public, 2-year","Public, 4-year or above"),
      size = "lg",
      width = "100%",
      direction = "horizontal",
      justified = F,
      individual = T,
      selected=F)
  })
  
  output$state <- renderUI({
    
    selectizeInput(
      inputId = "state",
      label = h3("Select State"),
      choices = c("--none--",sort(as.character(unique(fsums$state)))),
      multiple = FALSE,
      selected = "--none--"
    )
  })
  
  # Selectors ------------------------------
  
  
  # selector reactive: dname
  
  institution_choice<-reactive({
    
    c("--none--",sort(unique(fsums$dname)))
    
  })
  
  output$dname <- renderUI({
    
    selectizeInput(
      label=h3("Select University"),
      inputId='dname',
      selected = "--none--",
      choices= institution_choice()
      
    )
  })
  
  observeEvent({
    input$sector 
    input$state}, {
      
      # -----. Progress Bar at the bottom of the page
      
      withProgress(
        
        message='Please wait',
        detail='Running Model...',
        value=0, {
          n <- 2
          
          if(input$state=="--none--") {
            updateSelectizeInput(
              session,
              inputId = "dname",
              choices = c("--none--",
                          sort(fsums %>%
                                 filter(sector %in% input$sector) %>%
                                 distinct(dname) %>%
                                 pull(dname))),
              server=TRUE
            )
          }
          
          if(is.null(input$sector) & input$state!="--none--") {
            updateSelectizeInput(
              inputId = "dname",
              choices = c("--none--",sort(fsums %>%
                                            filter(state %in% input$state) %>%
                                            distinct(dname) %>%
                                            pull(dname))),
              server = TRUE
            )
          }
          
          if(!is.null(input$sector) & input$state!="--none--") {
            
            updateSelectizeInput(
              inputId = "dname",
              choices = c("--none--",sort(
                fsums %>%
                  filter(sector %in% input$sector) %>%
                  filter(state %in% input$state) %>%
                  distinct(dname) %>%
                  pull(dname))),
              server = TRUE
            )
          }
          
          if(is.null(input$sector) & input$state=="--none--") {
            
            updateSelectizeInput(
              inputId = "dname",
              choices = institution_choice())
            
          }
          
          # Prgregress Bar End.
          
          incProgress(1/n, detail = paste("Finished section 1"))
          Sys.sleep(0.5)
        })
    })
  
  
  #selected = NULL,
  #options = list(placeholder = 'Select University',
  #               onInitialize = I('function() { this.setValue(""); }')),
  #server = TRUE
  
  # -----. Compare selectizeinput
  
  updateSelectizeInput(
    inputId = "dname_comp",
    choices = c("--none--",
                fsums %>%
      distinct(dname) %>%
      arrange(dname) %>%
      pull(dname)),
    server = TRUE)
  
  observeEvent(input$dname, {
    
    sector_select<-reactive({
      
      fsums %>% 
        filter(dname==input$dname) %>%
        distinct(sector) %>%
        pull(sector)
    })
    
    updateSelectizeInput(
      inputId = "dname_comp",
      choices = c("--none--", fsums %>% 
        filter(sector %in% sector_select()) %>% 
        distinct(dname) %>% 
        arrange(dname) %>% 
        pull(dname)),
      server = TRUE
    )
  })
  
  # ----. Commpare Selectizeinput part 2
  
  updateSelectizeInput(
    inputId = "dname2_comp",
    choices = c("--none--",fsums %>% 
      distinct(dname) %>% 
      arrange(dname) %>% 
      pull(dname)),
    server = TRUE)
  
  observeEvent(input$dname, {
    
    sector_select<-reactive({
      
      fsums %>% 
        filter(dname==input$dname) %>%
        distinct(sector) %>%
        pull(sector)
    })
    
    updateSelectizeInput(
      inputId = "dname2_comp",
      choices = c("--none--", fsums %>% 
        filter(sector %in% sector_select()) %>% 
        distinct(dname) %>% 
        arrange(dname) %>% 
        pull(dname)),
      server = TRUE
    )
  })
  
  # ----. Commpare Selectizeinput part 3 

  updateSelectizeInput(
    inputId = "dname3_comp",
    choices = c("--none--", fsums %>%
      distinct(dname) %>%
      arrange(dname) %>%
      pull(dname)),
    server = TRUE)

  observeEvent(input$dname, {

    sector_select<-reactive({

      fsums %>%
        filter(dname==input$dname) %>%
        distinct(sector) %>%
        pull(sector)
    })

    updateSelectizeInput(
      inputId = "dname3_comp",
      choices = c("--none--", fsums %>%
        filter(sector %in% sector_select()) %>%
        distinct(dname) %>%
        arrange(dname) %>%
        pull(dname)),
      server = TRUE
    )
  })
  
  
  
  
  
  
 
  
  # -----. Sector and number of schools within sector
  num_schools <- reactive({
    
    efa_fsums_pell_compl_profile_equity %>%
      select(dname, sector) %>% 
      distinct() %>% 
      add_count(sector) %>% 
      filter(dname == input$dname) %>% 
      pull(n)
    
  })
  
  output$num_schools_in_sector <- renderText({
    validate(
      need(input$dname != "--none--", '')
    )
    
    paste("Percentile rank is shown within all", num_schools(), "schools included in the", sector_choice() ,"sector")
  })
  
  
  ## General Charts
  
  # -----. Score solid gauge chart
  # col_stops <- reactive({
  #   data.frame(
  #   q = c(.1,.2,.3,.4,.5,.6,.7,.8,.9),
  #   c = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c',
  #         '#fc4e2a','#e31a1c','#bd0026','#800026'),
  #   stringsAsFactors = FALSE
  # )
  # })
  
  col_stops <- reactive({
    data.frame(q = c(.9),
               c = c('#3d6c72')
    )
  })
  
  output$fhii_score_gauge <- renderHighchart({
    validate(
      need(input$dname != "--none--", 'Please Choose Institution')
    )
    
    
    highchart() %>% 
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '80%',
          shape = 'arc'
        ) 
      ) %>% 
      hc_yAxis(
        stops = list_parse2(col_stops()),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = -.2,
        max = .2,
        labels = list(y = 26, style = list(fontSize = "15px"))
      ) %>% 
      hc_add_series(
        data = {fsums_react() %>%
            distinct(dname, total) %>%
            pull(total) %>% unique()},
        y = -100,
        radius = "100%",
        innerRadius = "80%",
        dataLabels = list(
          y = -70,
          borderWidth = 0,
          useHTML = TRUE,
          style = list(fontSize = "60px")
        )
      )  %>%
      hc_size(height = 600) %>%
      hc_tooltip(enabled = FALSE) %>% 
      hc_title(text = "Score",
               margin = -100,
               style = list(color = "#3e6d72", fontSize = "60px", fontWeight = 900,
                            fontFamily = 'Roboto')
      ) %>% 
      hc_caption(
        style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                     fontWeight = 900, textTransform = "uppercase"),
        y = -150,
        align = "center"
      )
  })
  
  
  
  
  
  
  
  # -----. Pell pct in enrollment
  
  jsCode <- JS("function(events) {
                  return false;
              }")
  
  pell_enroll_pct<-reactive({
    
    efa_fsums_pell_compl_profile_equity %>% 
      ungroup() %>%
      select(dname, year,pell_pct_per_inst_year, nonpell_pct_per_inst_year) %>%
      drop_na %>% 
      group_by(dname) %>%
      #filter(year == "2019") %>%
      #Quang: I think this is where the error is coming from
      filter(year==max(year)) %>%
      distinct() %>%
      rename('Pell'=pell_pct_per_inst_year, 'Non-Pell'=nonpell_pct_per_inst_year) %>%
      pivot_longer(3:4,names_to="pell")
    #mutate(value=scales::percent(value)) %>%
    
  })
  
  output$pell_enroll_pie <- renderHighchart({
    
    validate(
      need(input$dname!="--none--", 'Please Choose Institution')
    )
    
    
    highchart() %>% 
      hc_add_series(
        data = {pell_enroll_pct() %>% ungroup() %>% 
            filter(dname == input$dname) %>% 
            #filter(year == "2019")},
            filter(year == max(year))},
        type = "pie",
        hcaes(x = pell, y = value),
        animation = T,
        size = '95%',
        innerSize = '75%',
        allowPointSelect = T,
        dataLabels = list(enabled = FALSE),
        point = list(events = list(legendItemClick = jsCode))
      ) %>% 
      hc_plotOptions(
        series = list(showInLegend = TRUE),
        dataLabels = list(enabled = FALSE)
      ) %>%
      hc_colors(c("#3d6c72", "#c0ddd8")) %>% 
      #hc_colors(c("#9e9947","#0685ed","#24aa90","#ed1606","#7e95ac","#e9b73f")) %>% 
      hc_tooltip(pointFormat = "{point.percentage:,.2f}%") %>% 
      hc_legend(
        y = -60) %>% 
      hc_title(
        text = "Pell Grant Status",
        style = list(fontSize = "14px", color = "black", fontFamily = "Roboto",
                     fontWeight = 900, textTransform = "uppercase"),
        y = 35,
        margin = 20
      )
    
    
  })
  
  # -----. racial pct in enrollment
  
  
  
  output$race_enroll_pie <- renderHighchart({
    
    validate(
      need(input$dname!="--none--", 'Please Choose Institution')
    )
    
    highchart() %>% 
      hc_add_series(
        data = {efa_fsums_pell_compl_profile_equity %>% ungroup() %>%
            select(dname, year, race_long, enrollment_pct_per_inst_year_race) %>%
            drop_na %>% 
            #filter(year == "2019") %>%
            filter(year==max(year)) %>%
            distinct() %>%
            filter(dname==input$dname)},
        type = "pie",
        hcaes(x = race_long, y = enrollment_pct_per_inst_year_race),
        animation = T,
        size = '88%',
        innerSize = '75%',
        allowPointSelect = T,
        dataLabels = list(enabled = FALSE),
        point = list(events = list(legendItemClick = jsCode))
      ) %>% 
      hc_plotOptions(
        series = list(showInLegend = TRUE)
      ) %>%
      #              AIAN       ASIAN   Black      Hispanic  NHPI      Race Unk  2 or more white
      hc_colors(c( "#9e9946", "#3e73c1","#25ab91","#ec2124","#7e95ac","#68576c","#ef8944","#e9b73f")) %>% 
      hc_tooltip(pointFormat = "{point.percentage:,.2f}%") %>% 
      hc_title(
        text = "Race",
        style = list(fontSize = "14px", color = "black", fontFamily = "Roboto",
                     fontWeight = 900, textTransform = "uppercase"),
        y=44
      ) %>% 
      hc_legend(
        y = -15
      )
    
  })
  
  
  #-----. Instructors pct pie
  
  output$instructor_pie <- renderHighchart({
    
    validate(
      need(input$dname!="--none--", 'Please Choose Institution')
    )
    
    highchart() %>% 
      hc_add_series(
        data = {efa_fsums_pell_compl_profile_equity %>% ungroup() %>%
            select(dname, year, race_long, instructor_pct_per_inst_year_race) %>%
            drop_na %>% 
            #filter(year == 2019) %>%
            filter(year==max(year)) %>%
            distinct() %>%
            filter(dname==input$dname)},
        type = "pie",
        hcaes(x = race_long, y = instructor_pct_per_inst_year_race),
        animation = T,
        size = '84%',
        innerSize = '75%',
        allowPointSelect = T,
        dataLabels = list(enabled = FALSE),
        point = list(events = list(legendItemClick = jsCode))
      ) %>% 
      hc_plotOptions(
        series = list(showInLegend = TRUE)
      ) %>%
      #              AIAN       ASIAN   Black      Hispanic  NHPI      Race Unk  2 or more white
      hc_colors(c( "#9e9946", "#3e73c1","#25ab91","#ec2124","#7e95ac","#68576c","#ef8944","#e9b73f")) %>% 
      hc_tooltip(pointFormat = "{point.percentage:,.2f}%") %>% 
      hc_title(
        text = "Instructors",
        style = list(fontSize = "14px", color = "black", fontFamily = "Roboto",
                     fontWeight = 900, textTransform = "uppercase"),
        y=44
      ) %>% 
      hc_legend(
        y = -15
      )
    
  })
  
  
  ## Completion Charts
  
  # -----. Completion generic reactive
  
  completion_generic_data<-reactive({
    
    efa_fsums_pell_compl_profile_equity %>% ungroup() %>%
      select(dname, year,completion_rate_per_inst_year, non_completion_rate_per_inst_year) %>%
      drop_na %>% 
      group_by(dname) %>%
      #filter(year == "2019") %>%
      filter(year==max(year)) %>%
      distinct() %>%
      pivot_longer(3:4, names_to = "completion", values_to = "value") %>%
      filter(dname==input$dname)
    
  })
  
  completion_generic_data_years <- reactive({
    efa_fsums_pell_compl_profile_equity %>% ungroup() %>%
      select(dname, year,completion_rate_per_inst_year, non_completion_rate_per_inst_year) %>%
      drop_na %>% 
      group_by(dname) %>%
      distinct() %>%
      pivot_longer(3:4, names_to = "completion", values_to = "value") %>%
      filter(dname==input$dname) %>% 
      filter(completion == "completion_rate_per_inst_year") %>% 
      ungroup()	%>% 
      select(year, value)
  })
  

  # -----. grand_compl = average completion rate per institution
  
  output$grand_compl_pie <- renderHighchart({
    
    validate(
      need(input$dname!="--none--", 'Please Choose Institution'),
      need(nrow(completion_generic_data())!=0, 'No Data Available')
    )
    
    labels<-htmltools::HTML(glue::glue("<B><FONT SIZE = +30><center> {scales::percent(completion_generic_data()$value[1])} </center></B></FONT>"))
    
    completion_generic_data() %>%
      hchart("pie", 
             hcaes(x=completion, y=value), 
             animation=T, 
             size= '100%',
             innerSize= '80%', 
             allowPointSelect=T, 
             dataLabels=list(enabled=FALSE, distance='-30%')) %>%
      hc_colors(c("#000000","#ffffff")) %>% 
      hc_tooltip(formatter= 
                   JS("function () { return ' <br /> Overall Completion Rate ' + (this.y.toFixed(2)*100) +'%' ;}")) %>%
      hc_labels(
        items = list(
          list(
            html = labels,
            style = list(
              fontSize = "16px",
              fontWeight = "bold",
              color = "#000000",
              left = "125%",
              top = "80%"
            )
          )
        )
      )
    
  })
  
  # -----. pell pct in completion: pell vs non-pell
  
  pell_compl_react_graph1<-reactive({
    
    completion_generic_data() %>%
      hchart("pie",
             hcaes(x=completion, y=value, color=c("#000000","#ffffff")), 
             animation=T,
             size= '70%',
             innerSize= '80%', 
             allowPointSelect=T,
             dataLabels=list(enabled=FALSE, distance='-30%')) %>%
      hc_colors(c("#000000","#ffffff")) %>% 
      hc_tooltip(formatter= 
                   JS("function () { return  (this.y.toFixed(2)*100) +'%' ;}")) 
    
  })
  
  pell_compl_react_data<-reactive({
    
    efa_fsums_pell_compl_profile_equity %>% ungroup() %>%
      select(dname, year, pell_compl_pct, pell_non_comple_pct) %>%
      group_by(dname) %>%
      #filter(year == 2019) %>%
      filter(year==max(year)) %>%
      distinct() %>%
      pivot_longer(3:4, names_to = "pell_compl", values_to = "value") %>%
      filter(dname==input$dname)
    
  })
  
  output$pell_compl_pie <- renderHighchart({
    
    validate(
      need(input$dname!="--none--", 'Please Choose Institution'),
      need(nrow(completion_generic_data())!=0, 'No Data Available')
    )
    
    labels<-htmltools::HTML(glue::glue("<B><FONT SIZE = +30><center> {scales::percent(pell_compl_react_data()$value[1])} </center></B></FONT>"))
    
    pell_compl_react_graph1() %>%
      hc_add_series(
        data={pell_compl_react_data()},
        type = "pie",
        hcaes(x=pell_compl, y=value, color=c("#3e6d72","#ffffff")),
        animation=T,
        size= '100%',
        innerSize= '75%',
        allowPointSelect=T,
        dataLabels=list(enabled=FALSE, distance='-30%')) %>% 
      # hc_tooltip(formatter= 
      #              JS("function () { return (this.y.toFixed(2)*100) +'%' ;}")) %>%
      hc_tooltip(formatter= 
                   JS("function () { return  (this.y*100).toFixed(2) +'%' ;}")) %>%
      hc_labels(
        items = list(
          list(
            html = labels,
            style = list(
              fontSize = "16px",
              fontWeight = "bold",
              color = "#3e6d72",
              left = "70%",
              top = "70%"
            )
          )
        )
      )
    
    
  })
  
  # -----. pell pct in completion: pell vs non-pell
  
  output$non_pell_compl_pie <- renderHighchart({
    
    validate(
      need(input$dname!="--none--", 'Please Choose Institution'),
      need(nrow(completion_generic_data())!=0, 'No Data Available')
    )
    
    labels<-htmltools::HTML(glue::glue("<B><FONT SIZE = +30><center> {scales::percent(pell_compl_react_data()$value[2])} </center></B></FONT>"))
    
    pell_compl_react_graph1() %>%
      hc_add_series(
        data={pell_compl_react_data() %>%
            arrange(desc(pell_compl))},
        type = "pie",
        hcaes(x=pell_compl, y=value, color=c("#bfdcd7","#ffffff")),
        animation=T,
        size= '100%',
        innerSize= '75%',
        allowPointSelect=T,
        dataLabels=list(enabled=FALSE)) %>%
      hc_tooltip(formatter= 
                   JS("function () { return (this.y*100).toFixed(2) +'%' ;}")) %>%
      hc_labels(
        items = list(
          list(
            html = labels,
            style = list(
              fontSize = "16px",
              fontWeight = "bold",
              color = "#bfdcd7",
              left = "70%",
              top = "70%"
            )
          )
        )
      )
    
  })
  
  ## Racial breakdown of completion rate
  
  # -----. Race compl reactive general
  
  race_compl_react_data<-reactive({
    
    efa_fsums_pell_compl_profile_equity %>% ungroup() %>%
      select(dname, year, race, completion_rate_per_inst_year_race, non_completion_rate_per_inst_year_race) %>%
      filter(dname==input$dname) %>%
      #mutate(non_completion_rate_per_inst_year_race=1-completion_rate_per_inst_year_race) %>%
      group_by(dname) %>%
      filter(year==max(year)) %>%
      mutate(non_completion_rate_per_inst_year_race = 1 - completion_rate_per_inst_year_race) %>% 
      distinct() %>%
      pivot_longer(4:5, names_to = "race_compl", values_to = "value") %>%
      #mutate(value=round(value, 2)) %>%
      mutate_at(vars(value), ~replace(., is.nan(.), NA))
    
  })
  
  # -----. white
  output$white_compl_pie <- renderHighchart({
    
    validate(
      need(input$dname!="--none--", 'Please Choose Institution'),
      need(nrow(race_compl_react_data())!=0, 'No Data Available')
    )
    
    if(is.na({race_compl_react_data() %>% ungroup() %>%
        filter(race=="White"&race_compl=="completion_rate_per_inst_year_race") %>%
        pull(value)})) return("N/A") 
    
    labels<-htmltools::HTML(glue::glue("<B><FONT SIZE = +30><center> {scales::percent(race_compl_react_data()$value[15])} </center></B></FONT>"))
    
    pell_compl_react_graph1() %>%
      hc_add_series(
        data={
          race_compl_react_data() %>% 
            ungroup() %>%
            filter(race=="White") %>%
            arrange(race_compl)
        },
        type = "pie",
        hcaes(x=race_compl, y=value, color=c("#e9b73f","#ffffff")),
        animation=T,
        size= '100%',
        innerSize= '75%',
        allowPointSelect=T,
        dataLabels=list(enabled=FALSE)
      ) %>% 
      hc_tooltip(formatter= 
                   JS("function () { return (this.y*100).toFixed(2) +'%' ;}")) %>%
      hc_labels(
        items = list(
          list(
            html = labels,
            style = list(
              fontSize = "16px",
              fontWeight = "bold",
              color = "#e9b73f",
              left = "70%",
              top = "70%"
            )
          )
        )
      )
    
  })
  
  
  
  # -----. Hispanic
  output$hispanic_compl_pie <- renderHighchart({
    
    validate(
      need(input$dname!="--none--", 'Please Choose Institution'),
      need(nrow(race_compl_react_data())!=0, 'No Data Available')
    )
    
    if(is.nan(race_compl_react_data() %>% ungroup() %>%
              filter(race=="Hispanic"&race_compl=="completion_rate_per_inst_year_race") %>%
              pull(value))) return()
    
    labels<-htmltools::HTML(glue::glue("<B><FONT SIZE = +30><center> {scales::percent(race_compl_react_data()$value[7])} </center></B></FONT>"))
    
    pell_compl_react_graph1() %>%
      hc_add_series(
        data={race_compl_react_data() %>% ungroup() %>%
            filter(race=="Hispanic") %>%
            arrange(race_compl)},
        type = "pie",
        hcaes(x=race_compl, y=value, color=c("#ec2124","#ffffff")),
        animation=T,
        size= '100%',
        innerSize= '75%',
        allowPointSelect=T,
        dataLabels=list(enabled=FALSE)) %>% 
      # hc_tooltip(formatter= 
      #              JS("function () { return (this.y.toFixed(2)*100) +'%' ;}")) %>%
      hc_tooltip(formatter= 
                   JS("function () { return (this.y*100).toFixed(2) +'%' ;}")) %>%
      hc_labels(
        items = list(
          list(
            html = labels,
            style = list(
              fontSize = "16px",
              fontWeight = "bold",
              color = "#ed1606",
              left = "70%",
              top = "70%"
            )
          )
        )
      )
    
  })
  
  # -----. AIAN
  output$aian_compl_pie <- renderHighchart({
    
    validate(
      need(input$dname!="--none--", 'Please Choose Institution'),
      need(nrow(race_compl_react_data())!=0, 'No Data Available')
    )
    
    if(is.nan(race_compl_react_data() %>% ungroup() %>%
              filter(race=="AIAN"&race_compl=="completion_rate_per_inst_year_race") %>%
              pull(value))) return()
    
    labels<-htmltools::HTML(glue::glue("<B><FONT SIZE = +30><center> {scales::percent(race_compl_react_data()$value[1])} </center></B></FONT>"))
    
    pell_compl_react_graph1() %>%
      hc_add_series(
        data={race_compl_react_data() %>% ungroup() %>%
            filter(race=="AIAN") %>%
            arrange(race_compl)},
        type = "pie",
        hcaes(x=race_compl, y=value, color=c("#9e9946","#ffffff")),
        animation=T,
        size= '100%',
        innerSize= '75%',
        allowPointSelect=T,
        dataLabels=list(enabled=FALSE))%>% 
      # hc_tooltip(formatter= 
      #              JS("function () { return (this.y.toFixed(2)*100) +'%' ;}")) %>%
      hc_tooltip(formatter= 
                   JS("function () { return (this.y*100).toFixed(2) +'%' ;}")) %>%
      hc_labels(
        items = list(
          list(
            html = labels,
            style = list(
              fontSize = "16px",
              fontWeight = "bold",
              color = "#9e9947",
              left = "70%",
              top = "70%"
            )
          )
        )
      )
    
  })
  
  # -----. Black
  output$black_compl_pie <- renderHighchart({
    
    validate(
      need(input$dname!="--none--", 'Please Choose Institution'),
      need(nrow(race_compl_react_data())!=0, 'No Data Available')
    )
    
    if(is.nan(race_compl_react_data() %>% ungroup() %>%
              filter(race=="Black"&race_compl=="completion_rate_per_inst_year_race") %>%
              pull(value))) return()
    
    labels<-htmltools::HTML(glue::glue("<B><FONT SIZE = +30><center> {scales::percent(race_compl_react_data()$value[5])} </center></B></FONT>"))
    
    pell_compl_react_graph1() %>%
      hc_add_series(
        data={race_compl_react_data() %>% ungroup() %>%
            filter(race=="Black") %>%
            arrange(race_compl)},
        type = "pie",
        hcaes(x=race_compl, y=value, color=c("#25ab91","#ffffff")),
        animation=T,
        size= '100%',
        innerSize= '75%',
        allowPointSelect=T,
        dataLabels=list(enabled=FALSE))%>% 
      # hc_tooltip(formatter= 
      #              JS("function () { return (this.y.toFixed(2)*100) +'%' ;}")) %>%
      hc_tooltip(formatter= 
                   JS("function () { return (this.y*100).toFixed(2) +'%' ;}")) %>%
      hc_labels(
        items = list(
          list(
            html = labels,
            style = list(
              fontSize = "16px",
              fontWeight = "bold",
              color = "#24aa90",
              left = "70%",
              top = "70%"
            )
          )
        )
      )
    
  })
  
  # -----. Asian
  output$asian_compl_pie <- renderHighchart({
    
    validate(
      need(input$dname!="--none--", 'Please Choose Institution'),
      need(nrow(race_compl_react_data())!=0, 'No Data Available')
    )
    
    if(is.na({race_compl_react_data() %>% ungroup() %>%
        filter(race=="Hispanic"&race_compl=="completion_rate_per_inst_year_race") %>%
        pull(value)})) return("N/A") 
    
    labels<-htmltools::HTML(glue::glue("<B><FONT SIZE = +30><center> {scales::percent(race_compl_react_data()$value[3])} </center></B></FONT>"))
    
    pell_compl_react_graph1() %>%
      hc_add_series(
        data={race_compl_react_data() %>% ungroup() %>%
            filter(race=="Asian") %>%
            arrange(race_compl)},
        type = "pie",
        hcaes(x=race_compl, y=value, color=c("#3e7ec1","#ffffff")),
        animation=T,
        size= '100%',
        innerSize= '75%',
        allowPointSelect=T,
        dataLabels=list(enabled=FALSE))%>% 
      # hc_tooltip(formatter= 
      #              JS("function () { return (this.y*100).toFixed(2) +'%' ;}")) %>%
      hc_tooltip(formatter= 
                   JS("function () { return (this.y*100).toFixed(2) +'%' ;}")) %>%
      hc_labels(
        items = list(
          list(
            html = labels,
            style = list(
              fontSize = "16px",
              fontWeight = "bold",
              color = "#0685ed",
              left = "70%",
              top = "70%"
            )
          )
        )
      )
    
  })
  
  # -----. NHPI
  output$nhpi_compl_pie <- renderHighchart({
    
    validate(
      need(input$dname!="--none--", 'Please Choose Institution'),
      need(nrow(race_compl_react_data())!=0, 'No Data Available')
    )
    
    if(is.nan(race_compl_react_data() %>% ungroup() %>%
              filter(race=="NHPI"&race_compl=="completion_rate_per_inst_year_race") %>%
              pull(value))) return()
    
    labels<-htmltools::HTML(glue::glue("<B><FONT SIZE = +30><center> {scales::percent(race_compl_react_data()$value[9])} </center></B></FONT>"))
    
    pell_compl_react_graph1() %>%
      hc_add_series(
        data={race_compl_react_data() %>% ungroup() %>%
            filter(race=="NHPI") %>%
            arrange(race_compl)},
        type = "pie",
        hcaes(x=race_compl, y=value, color=c("#7e95ac","#ffffff")),
        animation=T,
        size= '100%',
        innerSize= '75%',
        allowPointSelect=T,
        dataLabels=list(enabled=FALSE))%>% 
      # hc_tooltip(formatter= 
      #              JS("function () { return (this.y.toFixed(2)*100) +'%' ;}")) %>%
      hc_tooltip(formatter= 
                   JS("function () { return (this.y*100).toFixed(2) +'%' ;}")) %>%
      hc_labels(
        items = list(
          list(
            html = labels,
            style = list(
              fontSize = "16px",
              fontWeight = "bold",
              color = "#7e95ac",
              left = "70%",
              top = "70%"
            )
          )
        )
      )
    
  })
  
  
  # -----. Two or More Races
  output$two_or_more_compl_pie <- renderHighchart({
    
    validate(
      need(input$dname!="--none--", 'Please Choose Institution'),
      need(nrow(race_compl_react_data())!=0, 'No Data Available')
    )
    
    if(is.nan(race_compl_react_data() %>% ungroup() %>%
              filter(race=="Two or More Races"&race_compl=="completion_rate_per_inst_year_race") %>%
              pull(value))) return()
    
    labels<-htmltools::HTML(glue::glue("<B><FONT SIZE = +30><center> {scales::percent(race_compl_react_data()$value[13])} </center></B></FONT>"))
    
    pell_compl_react_graph1() %>%
      hc_add_series(
        data={race_compl_react_data() %>% ungroup() %>%
            filter(race=="Two or More Races") %>%
            arrange(race_compl)},
        type = "pie",
        hcaes(x=race_compl, y=value, color=c("#ef8944","#ffffff")),
        animation=T,
        size= '100%',
        innerSize= '75%',
        allowPointSelect=T,
        dataLabels=list(enabled=FALSE))%>% 
      # hc_tooltip(formatter= 
      #              JS("function () { return (this.y.toFixed(2)*100) +'%' ;}")) %>%
      hc_tooltip(formatter= 
                   JS("function () { return (this.y*100).toFixed(2) +'%' ;}")) %>%
      hc_labels(
        items = list(
          list(
            html = labels,
            style = list(
              fontSize = "16px",
              fontWeight = "bold",
              color = "#7e95ac",
              left = "70%",
              top = "70%"
            )
          )
        )
      )
    
  })
  
  
  #-----. Race Unknown
  
  output$race_unknown_compl_pie <- renderHighchart({
    
    validate(
      need(input$dname!="--none--", 'Please Choose Institution'),
      need(nrow(race_compl_react_data())!=0, 'No Data Available')
    )
    
    if(is.nan(race_compl_react_data() %>% ungroup() %>%
              filter(race=="Race Unknown"&race_compl=="completion_rate_per_inst_year_race") %>%
              pull(value))) return()
    
    labels<-htmltools::HTML(glue::glue("<B><FONT SIZE = +30><center> {scales::percent(race_compl_react_data()$value[11])} </center></B></FONT>"))
    
    pell_compl_react_graph1() %>%
      hc_add_series(
        data={race_compl_react_data() %>% ungroup() %>%
            filter(race=="Race Unknown") %>%
            arrange(race_compl)},
        type = "pie",
        hcaes(x=race_compl, y=value, color=c("#68576c","#ffffff")),
        animation=T,
        size= '100%',
        innerSize= '75%',
        allowPointSelect=T,
        dataLabels=list(enabled=FALSE))%>% 
      # hc_tooltip(formatter= 
      #              JS("function () { return (this.y.toFixed(2)*100) +'%' ;}")) %>%
      hc_tooltip(formatter= 
                   JS("function () { return (this.y*100).toFixed(2) +'%' ;}")) %>%
      hc_labels(
        items = list(
          list(
            html = labels,
            style = list(
              fontSize = "16px",
              fontWeight = "bold",
              color = "#7e95ac",
              left = "70%",
              top = "70%"
            )
          )
        )
      )
    
  })
  
  
  #-----. Equity Compare Sub-tab
  efa_fsums_pell_compl_profile_equity <- efa_fsums_pell_compl_profile_equity %>% 
    mutate("race_long" = race) %>% 
    mutate_at(c("race_long"), str_replace_all, pattern = "AIAN", replacement = "American Indian and Alaskan Native") %>% 
    mutate_at(c("race_long"), str_replace_all, pattern = "NHPI", replacement = "Native Hawaiian and Pacific Islander") %>% 
    mutate(enrollment_pct_per_inst_year_race_2 = enrollment_pct_per_inst_year_race*100) %>% 
    mutate(instructor_pct_per_inst_year_race_2 = instructor_pct_per_inst_year_race*100) %>% 
    mutate(pop_pct_per_state_year_race_2 = pop_pct_per_state_year_race*100) %>% 
    mutate(pop_pct_per_natl_year_race_2 = pop_pct_per_natl_year_race*100) %>% 
    mutate(completers_pct_per_inst_year_race_2 = completers_pct_per_inst_year_race*100) %>% 
    mutate(completion_rate_per_inst_year_race_2 = completion_rate_per_inst_year_race*100)
  
  bar_data <- reactive({
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname == input$dname) %>% 
      filter(year == max(year)) %>% 
      select("dname", "race_long", "completers_pct_per_inst_year_race",
             "enrollment_pct_per_inst_year_race",
             "instructor_pct_per_inst_year_race",
             "pop_pct_per_state_year_race",
             "pop_pct_per_natl_year_race",
             "completion_rate_per_inst_year_race"
      ) %>% 
      mutate(drilldown = tolower(race_long)) %>% 
      mutate(enrollment_pct_per_inst_year_race_1 = enrollment_pct_per_inst_year_race*100) %>% 
      mutate(instructor_pct_per_inst_year_race_1 = instructor_pct_per_inst_year_race*100) %>% 
      mutate(pop_pct_per_state_year_race_1 = pop_pct_per_state_year_race*100) %>% 
      mutate(pop_pct_per_natl_year_race_1 = pop_pct_per_natl_year_race*100) %>% 
      mutate(completers_pct_per_inst_year_race_1 = completers_pct_per_inst_year_race*100) %>% 
      mutate(completion_rate_per_inst_year_race_1 = completion_rate_per_inst_year_race*100) %>% 
      distinct()
    
  })
  
  averages <- reactive({
    
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname == input$dname) %>% 
      #filter(year == c("2014", "2015", "2016", "2017", "2018", "2019")) %>% 
      select("dname", "race_long", "completers_pct_per_inst_year_race",
             "enrollment_pct_per_inst_year_race",
             "instructor_pct_per_inst_year_race",
             "pop_pct_per_state_year_race",
             "pop_pct_per_natl_year_race",
             "completion_rate_per_inst_year_race"
      ) %>% 
      group_by(race_long) %>% 
      summarise(across(c("enrollment_pct_per_inst_year_race", 
                         "instructor_pct_per_inst_year_race",
                         "pop_pct_per_state_year_race",
                         "pop_pct_per_natl_year_race",
                         "completers_pct_per_inst_year_race",
                         "completion_rate_per_inst_year_race"), list(mean))) %>% 
      mutate(drilldown = tolower(race_long)) %>% 
      mutate(enrollment_pct_per_inst_year_race_1 = enrollment_pct_per_inst_year_race_1*100) %>% 
      mutate(instructor_pct_per_inst_year_race_1 = instructor_pct_per_inst_year_race_1*100) %>% 
      mutate(pop_pct_per_state_year_race_1 = pop_pct_per_state_year_race_1*100) %>% 
      mutate(pop_pct_per_natl_year_race_1 = pop_pct_per_natl_year_race_1*100) %>% 
      mutate(completers_pct_per_inst_year_race_1 = completers_pct_per_inst_year_race_1*100) %>% 
      mutate(completion_rate_per_inst_year_race_1 = completion_rate_per_inst_year_race_1*100)
    
  })
  
  output$enrollment_bar <- renderHighchart({
    validate(
      need(input$dname!="--none--", 'Please Choose Institution'),
      need(nrow(race_compl_react_data())!=0, 'No Data Available')
    )
    
    
    fn <-"function(e){
            var chart = this,
            drilldowns = chart.userOptions.drilldown.series,
            series = [];
          e.preventDefault();
          Highcharts.each(drilldowns, function(p, i) {
            if (p.id.includes(e.point.name)) {
              chart.addSingleSeriesAsDrilldown(e.point, p);
            }
          });
          chart.applyDrilldown();
          
                    var chart = this;
                          setTimeout(function() {
                            chart.update({
                              chart: {
                                inverted: false
                              }
                            });
                          }, 0);
                          

          
}"
    
    fn2 <- "function(){
      var chart = this;
      setTimeout(function() {
      chart.update({
        chart: {
          inverted: true
        }
    });
  }, 0);

}"
    
    fn3 <- "function(e){
      
      this.xAxis[0].setTitle({
            text: 'Mean Absolute Error (in days)'
    }"
    
    
    highchart() %>%
      hc_chart(type = "column",
               inverted = TRUE,
               events = list(
                 drilldown = JS(fn),
                 drillupall = JS(fn2)
               ),
               marginLeft = 285) %>%
      hc_title(
        text = "Enrollment by Race compared to State and National race demographics of 18 - 24 year olds",
        style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                     fontWeight = 900, textTransform = "uppercase")
      ) %>% 
      hc_yAxis(
        labels = list(
          overflow = "allow",
          padding = 100
        ) 
      ) %>% 
      hc_subtitle(text = "Select any race to see the trend over time for that race") %>% 
      hc_plotOptions(column = list(grouping = FALSE, shadow = FALSE)) %>% 
      hc_tooltip(valueDecimals = 1,
                 shared = TRUE,
                 split = FALSE,
                 pointFormat= "</b>{point.series.name}: {point.y}% <br>") %>% 
      hc_xAxis(type = "category") %>%
      hc_add_series(
        name = "2021 National Percent",
        color = "#bfdcd7",
        pointPadding = -.2,
        data = list(list(name = "American Indian and Alaskan Native",
                         y = {bar_data() %>% 
                             filter(race_long == "American Indian and Alaskan Native") %>% 
                             pull("pop_pct_per_natl_year_race_1")},
                         drilldown = "American Indian and Alaskan Native"),
                    list(name = "Asian",
                         y = {bar_data() %>% 
                             filter(race_long == "Asian") %>% 
                             pull("pop_pct_per_natl_year_race_1")},
                         drilldown = "Asian"),
                    list(name = "Black",
                         y = {bar_data() %>% 
                             filter(race_long == "Black") %>% 
                             pull("pop_pct_per_natl_year_race_1")},
                         drilldown = "Black"),
                    list(name = "Hispanic",
                         y = {bar_data() %>% 
                             filter(race_long == "Hispanic") %>% 
                             pull("pop_pct_per_natl_year_race_1")},
                         drilldown = "Hispanic"),
                    list(name = "Native Hawaiian and Pacific Islander",
                         y = {bar_data() %>% 
                             filter(race_long == "Native Hawaiian and Pacific Islander") %>% 
                             pull("pop_pct_per_natl_year_race_1")},
                         drilldown = "Native Hawaiian and Pacific Islander"),
                    list(name = "Race Unknown",
                         y = {bar_data() %>% 
                             filter(race_long == "Race Unknown") %>% 
                             pull("pop_pct_per_natl_year_race_1")},
                         drilldown = "Race Unknown"),
                    list(name = "Two or More Races",
                         y = {bar_data() %>% 
                             filter(race_long == "Two or More Races") %>% 
                             pull("pop_pct_per_natl_year_race_1")},
                         drilldown = "Two or More Races"),
                    list(name = "White",
                         y = {bar_data() %>% 
                             filter(race_long == "White") %>% 
                             pull("pop_pct_per_natl_year_race_1")},
                         drilldown = "White")
        )
      ) %>% 
      hc_add_series(
        name = "2021 State Percent",
        color = "#3e6d72",
        pointPadding = 0,
        type = "column",
        data = bar_data()$pop_pct_per_state_year_race_1
      ) %>%
      hc_add_series(
        name = "2021 Institution Percent",
        color = "black",
        pointPadding = .3,
        type = "column",
        data = bar_data()$enrollment_pct_per_inst_year_race_1
      ) %>%
      hc_drilldown(allowPointDrilldown = TRUE,
                   activeAxisLabelStyle = list(text = "does it work?"),
                   series = list(
                     list(
                       name = "AIAN Institutional Percent",
                       id= "American Indian and Alaskan Native",
                       color = "black",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "American Indian and Alaskan Native") %>% 
                                             select("year", "enrollment_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "AIAN State Percent",
                          color = "#3e6d72",
                          id= "American Indian and Alaskan Native",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "American Indian and Alaskan Native") %>% 
                                                select("year", "pop_pct_per_state_year_race_2"))}
                     ),
                     list(name = "AIAN National Percent",
                          color = "#bfdcd7",
                          id= "American Indian and Alaskan Native",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "American Indian and Alaskan Native") %>% 
                                                select("year", "pop_pct_per_natl_year_race_2"))}
                     ),
                     list(
                       name = "Asian Institutional Percent",
                       id= "Asian",
                       color = "black",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Asian") %>% 
                                             select("year", "enrollment_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Asian State Percent",
                          color = "#3e6d72",
                          id= "Asian",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Asian") %>% 
                                                select("year", "pop_pct_per_state_year_race_2"))}
                     ),
                     list(name = "Asian National Percent",
                          color = "#bfdcd7",
                          id= "Asian",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Asian") %>% 
                                                select("year", "pop_pct_per_natl_year_race_2"))}
                     ),
                     list(
                       name = "Black Institutional Percent",
                       color = "black",
                       id= "Black",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Black") %>% 
                                             select("year", "enrollment_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Black State Percent",
                          color = "#3e6d72",
                          id= "Black",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Black") %>% 
                                                select("year", "pop_pct_per_state_year_race_2"))}
                     ),
                     list(name = "Black National Percent",
                          color = "#bfdcd7",
                          id= "Black",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Black") %>% 
                                                select("year", "pop_pct_per_natl_year_race_2"))}
                     ),
                     list(
                       name = "Hispanic Institutional Percent",
                       color = "black",
                       id= "Hispanic",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Hispanic") %>% 
                                             select("year", "enrollment_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Hispanic State Percent",
                          color = "#3e6d72",
                          id= "Hispanic",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Hispanic") %>% 
                                                select("year", "pop_pct_per_state_year_race_2"))}
                     ),
                     list(name = "Hispanic National Percent",
                          color = "#bfdcd7",
                          id= "Hispanic",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Hispanic") %>% 
                                                select("year", "pop_pct_per_natl_year_race_2"))}
                     ),
                     list(
                       name = "NHPI Institutional Percent",
                       color = "black",
                       id= "Native Hawaiian and Pacific Islander",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Native Hawaiian and Pacific Islander") %>% 
                                             select("year", "enrollment_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "NHPI State Percent",
                          color = "#3e6d72",
                          id= "Native Hawaiian and Pacific Islander",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Native Hawaiian and Pacific Islander") %>% 
                                                select("year", "pop_pct_per_state_year_race_2"))}
                     ),
                     list(name = "NHPI National Percent",
                          color = "#bfdcd7",
                          id= "Native Hawaiian and Pacific Islander",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Native Hawaiian and Pacific Islander") %>% 
                                                select("year", "pop_pct_per_natl_year_race_2"))}
                     ),
                     list(
                       name = "White Institutional Percent",
                       color = "black",
                       id= "White",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "White") %>% 
                                             select("year", "enrollment_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "White State Percent",
                          color = "#3e6d72",
                          id= "White",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "White") %>% 
                                                select("year", "pop_pct_per_state_year_race_2"))}
                     ),
                     list(name = "White National Percent",
                          color = "#bfdcd7",
                          id= "White",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "White") %>% 
                                                select("year", "pop_pct_per_natl_year_race_2"))}
                     ),
                     list(
                       name = "Two or More Races Institutional Percent",
                       color = "black",
                       id= "Two or More Races",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Two or More Races") %>% 
                                             select("year", "enrollment_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Two or More Races State Percent",
                          color = "#3e6d72",
                          id= "Two or More Races",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Two or More Races") %>% 
                                                select("year", "pop_pct_per_state_year_race_2"))}
                     ),
                     list(name = "Two or More Races National Percent",
                          color = "#bfdcd7",
                          id= "Two or More Races",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Two or More Races") %>% 
                                                select("year", "pop_pct_per_natl_year_race_2"))}
                     ),
                     list(
                       name = "Race Unknown Institutional Percent",
                       color = "black",
                       id= "Race Unknown",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Race Unknown") %>% 
                                             select("year", "enrollment_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Race Unknown State Percent",
                          color = "#3e6d72",
                          id= "Race Unknown",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Race Unknown") %>% 
                                                select("year", "pop_pct_per_state_year_race_2"))}
                     ),
                     list(name = "Race Unknown National Percent",
                          color = "#bfdcd7",
                          id= "Race Unknown",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Race Unknown") %>% 
                                                select("year", "pop_pct_per_natl_year_race_2"))}
                     )))
  })
  
  
  
  
  output$completers_bar <- renderHighchart({
    validate(
      need(input$dname!="--none--", 'Please Choose Institution'),
      need(nrow(race_compl_react_data())!=0, 'No Data Available')
    )
    
    
    fn <-"function(e){
            var chart = this,
            drilldowns = chart.userOptions.drilldown.series,
            series = [];
          e.preventDefault();
          Highcharts.each(drilldowns, function(p, i) {
            if (p.id.includes(e.point.name)) {
              chart.addSingleSeriesAsDrilldown(e.point, p);
            }
          });
          chart.applyDrilldown();
          
                    var chart = this;
                          setTimeout(function() {
                            chart.update({
                              chart: {
                                inverted: false
                              }
                            });
                          }, 0);
                          

          
}"
    
    fn2 <- "function(){
      var chart = this;
      setTimeout(function() {
      chart.update({
        chart: {
          inverted: true
        }
    });
  }, 0);

}"
    
    fn3 <- "function(e){
      
      this.xAxis[0].setTitle({
            text: 'Mean Absolute Error (in days)'
    }"
    
    
    highchart() %>%
      hc_chart(type = "column",
               inverted = TRUE,
               events = list(
                 drilldown = JS(fn),
                 drillupall = JS(fn2)
               ),
               marginLeft = 285) %>%
      hc_title(
        text = "Racial Breakdown of Graduating Students Compared to State and National Race Demographics of 18 - 24 year olds",
        style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                     fontWeight = 900, textTransform = "uppercase")
      ) %>%
      hc_subtitle(
        text = paste("", "Select any race to see the trend over time for that race")
      ) %>% 
      hc_plotOptions(column = list(grouping = FALSE, shadow = FALSE)) %>% 
      hc_tooltip(valueDecimals = 1,
                 shared = TRUE,
                 split = FALSE,
                 pointFormat= "</b>{point.series.name}: {point.y}% <br>") %>% 
      hc_xAxis(type = "category") %>%
      hc_add_series(
        name = "2021 National Percent",
        color = "#bfdcd7",
        pointPadding = -.2,
        data = list(list(name = "American Indian and Alaskan Native",
                         y = {bar_data() %>% 
                             filter(race_long == "American Indian and Alaskan Native") %>% 
                             pull("pop_pct_per_natl_year_race_1")},
                         drilldown = "American Indian and Alaskan Native"),
                    list(name = "Asian",
                         y = {bar_data() %>% 
                             filter(race_long == "Asian") %>% 
                             pull("pop_pct_per_natl_year_race_1")},
                         drilldown = "Asian"),
                    list(name = "Black",
                         y = {bar_data() %>% 
                             filter(race_long == "Black") %>% 
                             pull("pop_pct_per_natl_year_race_1")},
                         drilldown = "Black"),
                    list(name = "Hispanic",
                         y = {bar_data() %>% 
                             filter(race_long == "Hispanic") %>% 
                             pull("pop_pct_per_natl_year_race_1")},
                         drilldown = "Hispanic"),
                    list(name = "Native Hawaiian and Pacific Islander",
                         y = {bar_data() %>% 
                             filter(race_long == "Native Hawaiian and Pacific Islander") %>% 
                             pull("pop_pct_per_natl_year_race_1")},
                         drilldown = "Native Hawaiian and Pacific Islander"),
                    list(name = "Race Unknown",
                         y = {bar_data() %>% 
                             filter(race_long == "Race Unknown") %>% 
                             pull("pop_pct_per_natl_year_race_1")},
                         drilldown = "Race Unknown"),
                    list(name = "Two or More Races",
                         y = {bar_data() %>% 
                             filter(race_long == "Two or More Races") %>% 
                             pull("pop_pct_per_natl_year_race_1")},
                         drilldown = "Two or More Races"),
                    list(name = "White",
                         y = {bar_data() %>% 
                             filter(race_long == "White") %>% 
                             pull("pop_pct_per_natl_year_race_1")},
                         drilldown = "White")
        )
      ) %>% 
      hc_add_series(
        name = "2021 State Percent",
        color = "#3e6d72",
        pointPadding = 0,
        type = "column",
        data = bar_data()$pop_pct_per_state_year_race_1
      ) %>%
      hc_add_series(
        name = "2021 Institution Percent",
        color = "black",
        pointPadding = .3,
        type = "column",
        data = bar_data()$completers_pct_per_inst_year_race_1
      ) %>%
      hc_drilldown(allowPointDrilldown = TRUE,
                   activeAxisLabelStyle = list(text = "does it work?"),
                   series = list(
                     list(
                       name = "AIAN Institutional Percent",
                       id= "American Indian and Alaskan Native",
                       color = "black",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "American Indian and Alaskan Native") %>% 
                                             select("year", "completers_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "AIAN State Percent",
                          color = "#3e6d72",
                          id= "American Indian and Alaskan Native",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "American Indian and Alaskan Native") %>% 
                                                select("year", "pop_pct_per_state_year_race_2"))}
                     ),
                     list(name = "AIAN National Percent",
                          color = "#bfdcd7",
                          id= "American Indian and Alaskan Native",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "American Indian and Alaskan Native") %>% 
                                                select("year", "pop_pct_per_natl_year_race_2"))}
                     ),
                     list(
                       name = "Asian Institutional Percent",
                       id= "Asian",
                       color = "black",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Asian") %>% 
                                             select("year", "completers_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Asian State Percent",
                          color = "#3e6d72",
                          id= "Asian",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Asian") %>% 
                                                select("year", "pop_pct_per_state_year_race_2"))}
                     ),
                     list(name = "Asian National Percent",
                          color = "#bfdcd7",
                          id= "Asian",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Asian") %>% 
                                                select("year", "pop_pct_per_natl_year_race_2"))}
                     ),
                     list(
                       name = "Black Institutional Percent",
                       color = "black",
                       id= "Black",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Black") %>% 
                                             select("year", "completers_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Black State Percent",
                          color = "#3e6d72",
                          id= "Black",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Black") %>% 
                                                select("year", "pop_pct_per_state_year_race_2"))}
                     ),
                     list(name = "Black National Percent",
                          color = "#bfdcd7",
                          id= "Black",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Black") %>% 
                                                select("year", "pop_pct_per_natl_year_race_2"))}
                     ),
                     list(
                       name = "Hispanic Institutional Percent",
                       color = "black",
                       id= "Hispanic",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Hispanic") %>% 
                                             select("year", "completers_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Hispanic State Percent",
                          color = "#3e6d72",
                          id= "Hispanic",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Hispanic") %>% 
                                                select("year", "pop_pct_per_state_year_race_2"))}
                     ),
                     list(name = "Hispanic National Percent",
                          color = "#bfdcd7",
                          id= "Hispanic",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Hispanic") %>% 
                                                select("year", "pop_pct_per_natl_year_race_2"))}
                     ),
                     list(
                       name = "NHPI Institutional Percent",
                       color = "black",
                       id= "Native Hawaiian and Pacific Islander",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Native Hawaiian and Pacific Islander") %>% 
                                             select("year", "completers_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "NHPI State Percent",
                          color = "#3e6d72",
                          id= "Native Hawaiian and Pacific Islander",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Native Hawaiian and Pacific Islander") %>% 
                                                select("year", "pop_pct_per_state_year_race_2"))}
                     ),
                     list(name = "NHPI National Percent",
                          color = "#bfdcd7",
                          id= "Native Hawaiian and Pacific Islander",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Native Hawaiian and Pacific Islander") %>% 
                                                select("year", "pop_pct_per_natl_year_race_2"))}
                     ),
                     list(
                       name = "White Institutional Percent",
                       color = "black",
                       id= "White",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "White") %>% 
                                             select("year", "completers_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "White State Percent",
                          color = "#3e6d72",
                          id= "White",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "White") %>% 
                                                select("year", "pop_pct_per_state_year_race_2"))}
                     ),
                     list(name = "White National Percent",
                          color = "#bfdcd7",
                          id= "White",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "White") %>% 
                                                select("year", "pop_pct_per_natl_year_race_2"))}
                     ),
                     list(
                       name = "Two or More Races Institutional Percent",
                       color = "black",
                       id= "Two or More Races",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Two or More Races") %>% 
                                             select("year", "completers_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Two or More Races State Percent",
                          color = "#3e6d72",
                          id= "Two or More Races",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Two or More Races") %>% 
                                                select("year", "pop_pct_per_state_year_race_2"))}
                     ),
                     list(name = "Two or More Races National Percent",
                          color = "#bfdcd7",
                          id= "Two or More Races",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Two or More Races") %>% 
                                                select("year", "pop_pct_per_natl_year_race_2"))}
                     ),
                     list(
                       name = "Race Unknown Institutional Percent",
                       color = "black",
                       id= "Race Unknown",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Race Unknown") %>% 
                                             select("year", "completers_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Race Unknown State Percent",
                          color = "#3e6d72",
                          id= "Race Unknown",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Race Unknown") %>% 
                                                select("year", "pop_pct_per_state_year_race_2"))}
                     ),
                     list(name = "Race Unknown National Percent",
                          color = "#bfdcd7",
                          id= "Race Unknown",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Race Unknown") %>% 
                                                select("year", "pop_pct_per_natl_year_race_2"))}
                     )))
  })
  
  output$instructor_enrollment_bar <- renderHighchart({
    validate(
      need(input$dname!="--none--", 'Please Choose Institution'),
      need(nrow(race_compl_react_data())!=0, 'No Data Available')
    )
    
    
    fn <-"function(e){
            var chart = this,
            drilldowns = chart.userOptions.drilldown.series,
            series = [];
          e.preventDefault();
          Highcharts.each(drilldowns, function(p, i) {
            if (p.id.includes(e.point.name)) {
              chart.addSingleSeriesAsDrilldown(e.point, p);
            }
          });
          chart.applyDrilldown();
          
                    var chart = this;
                          setTimeout(function() {
                            chart.update({
                              chart: {
                                inverted: false
                              }
                            });
                          }, 0);
                          

          
}"
    
    fn2 <- "function(){
      var chart = this;
      setTimeout(function() {
      chart.update({
        chart: {
          inverted: true
        }
    });
  }, 0);

}"
    
    fn3 <- "function(e){
      
      this.xAxis[0].setTitle({
            text: 'Mean Absolute Error (in days)'
    }"
    
    
    highchart() %>%
      hc_chart(type = "column",
               inverted = TRUE,
               events = list(
                 drilldown = JS(fn),
                 drillupall = JS(fn2)
               ),
               marginLeft = 285) %>%
      hc_title(
        text = "Institutional Instructor Percentages Versus Enrollment Percentages by Race",
        style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                     fontWeight = 900, textTransform = "uppercase")
      ) %>% 
      hc_subtitle(text = "Select any race to see the trend over time for that race") %>% 
      hc_plotOptions(column = list(grouping = FALSE, shadow = FALSE)) %>% 
      hc_tooltip(valueDecimals = 1,
                 shared = TRUE,
                 split = FALSE,
                 pointFormat= "</b>{point.series.name}: {point.y}% <br>") %>% 
      hc_xAxis(type = "category") %>%
      hc_add_series(
        name = "2021 Instructor Percent",
        color = "#bfdcd7",
        pointPadding = -.2,
        data = list(list(name = "American Indian and Alaskan Native",
                         y = {bar_data() %>% 
                             filter(race_long == "American Indian and Alaskan Native") %>% 
                             pull("instructor_pct_per_inst_year_race_1")},
                         drilldown = "American Indian and Alaskan Native"),
                    list(name = "Asian",
                         y = {bar_data() %>% 
                             filter(race_long == "Asian") %>% 
                             pull("instructor_pct_per_inst_year_race_1")},
                         drilldown = "Asian"),
                    list(name = "Black",
                         y = {bar_data() %>% 
                             filter(race_long == "Black") %>% 
                             pull("instructor_pct_per_inst_year_race_1")},
                         drilldown = "Black"),
                    list(name = "Hispanic",
                         y = {bar_data() %>% 
                             filter(race_long == "Hispanic") %>% 
                             pull("instructor_pct_per_inst_year_race_1")},
                         drilldown = "Hispanic"),
                    list(name = "Native Hawaiian and Pacific Islander",
                         y = {bar_data() %>% 
                             filter(race_long == "Native Hawaiian and Pacific Islander") %>% 
                             pull("instructor_pct_per_inst_year_race_1")},
                         drilldown = "Native Hawaiian and Pacific Islander"),
                    list(name = "Race Unknown",
                         y = {bar_data() %>% 
                             filter(race_long == "Race Unknown") %>% 
                             pull("instructor_pct_per_inst_year_race_1")},
                         drilldown = "Race Unknown"),
                    list(name = "Two or More Races",
                         y = {bar_data() %>% 
                             filter(race_long == "Two or More Races") %>% 
                             pull("instructor_pct_per_inst_year_race_1")},
                         drilldown = "Two or More Races"),
                    list(name = "White",
                         y = {bar_data() %>% 
                             filter(race_long == "White") %>% 
                             pull("instructor_pct_per_inst_year_race_1")},
                         drilldown = "White")
        )
      ) %>% 
      hc_add_series(
        name = "2021 Enrollment Percent",
        color = "#3e6d72",
        pointPadding = 0,
        type = "column",
        data = bar_data()$enrollment_pct_per_inst_year_race_1
      ) %>%
      hc_drilldown(allowPointDrilldown = TRUE,
                   activeAxisLabelStyle = list(text = "does it work?"),
                   series = list(
                     list(
                       name = "AIAN Instructor Percent",
                       id= "American Indian and Alaskan Native",
                       color = "black",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "American Indian and Alaskan Native") %>% 
                                             select("year", "instructor_pct_per_inst_year_race_2" ))}
                       
                     ),
                     list(name = "AIAN Enrollment Percent",
                          color = "#3e6d72",
                          id= "American Indian and Alaskan Native",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "American Indian and Alaskan Native") %>% 
                                                select("year", "enrollment_pct_per_inst_year_race_2"))}
                     ),
                     list(
                       name = "Asian Instructor Percent",
                       id= "Asian",
                       color = "black",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Asian") %>% 
                                             select("year", "instructor_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Asian Enrollment Percent",
                          color = "#3e6d72",
                          id= "Asian",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Asian") %>% 
                                                select("year", "enrollment_pct_per_inst_year_race_2"))}
                     ),
                     list(
                       name = "Black Instructor Percent",
                       color = "black",
                       id= "Black",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Black") %>% 
                                             select("year", "instructor_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Black Enrollment Percent",
                          color = "#3e6d72",
                          id= "Black",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Black") %>% 
                                                select("year", "enrollment_pct_per_inst_year_race_2"))}
                     ),
                     list(
                       name = "Hispanic Instructor Percent",
                       color = "black",
                       id= "Hispanic",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Hispanic") %>% 
                                             select("year", "instructor_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Hispanic Enrollment Percent",
                          color = "#3e6d72",
                          id= "Hispanic",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Hispanic") %>% 
                                                select("year", "enrollment_pct_per_inst_year_race_2"))}
                     ),
                     list(
                       name = "NHPI Instructor Percent",
                       color = "black",
                       id= "Native Hawaiian and Pacific Islander",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Native Hawaiian and Pacific Islander") %>% 
                                             select("year", "instructor_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "NHPI Enrollment Percent",
                          color = "#3e6d72",
                          id= "Native Hawaiian and Pacific Islander",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Native Hawaiian and Pacific Islander") %>% 
                                                select("year", "enrollment_pct_per_inst_year_race_2"))}
                     ),
                     list(
                       name = "White Instructor Percent",
                       color = "black",
                       id= "White",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "White") %>% 
                                             select("year", "instructor_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "White Enrollment Percent",
                          color = "#3e6d72",
                          id= "White",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "White") %>% 
                                                select("year", "enrollment_pct_per_inst_year_race_2"))}
                     ),
                     list(
                       name = "Two or More Races Instructor Percent",
                       color = "black",
                       id= "Two or More Races",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Two or More Races") %>% 
                                             select("year", "instructor_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Two or More Races Enrollment Percent",
                          color = "#3e6d72",
                          id= "Two or More Races",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Two or More Races") %>% 
                                                select("year", "enrollment_pct_per_inst_year_race_2"))}
                     ),
                     list(
                       name = "Race Unknown Instructor Percent",
                       color = "black",
                       id= "Race Unknown",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Race Unknown") %>% 
                                             select("year", "instructor_pct_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Race Unknown Enrollment Percent",
                          color = "#3e6d72",
                          id= "Race Unknown",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Race Unknown") %>% 
                                                select("year", "enrollment_pct_per_inst_year_race_2"))}
                     )))
  })
  
  output$enrollment_completion_bar <- renderHighchart({
    validate(
      need(input$dname!="--none--", 'Please Choose Institution'),
      need(nrow(race_compl_react_data())!=0, 'No Data Available')
    )
    
    
    fn <-"function(e){
            var chart = this,
            drilldowns = chart.userOptions.drilldown.series,
            series = [];
          e.preventDefault();
          Highcharts.each(drilldowns, function(p, i) {
            if (p.id.includes(e.point.name)) {
              chart.addSingleSeriesAsDrilldown(e.point, p);
            }
          });
          chart.applyDrilldown();
          
                    var chart = this;
                          setTimeout(function() {
                            chart.update({
                              chart: {
                                inverted: false
                              }
                            });
                          }, 0);
                          

          
}"
    
    fn2 <- "function(){
      var chart = this;
      setTimeout(function() {
      chart.update({
        chart: {
          inverted: true
        }
    });
  }, 0);

}"
    
    fn3 <- "function(e){
      
      this.xAxis[0].setTitle({
            text: 'Mean Absolute Error (in days)'
    }"
    
    
    highchart() %>%
      hc_chart(type = "column",
               inverted = TRUE,
               events = list(
                 drilldown = JS(fn),
                 drillupall = JS(fn2)
               ),
               marginLeft = 285) %>%
      hc_title(
        text = "Institutional Enrollment percentages versus Completion Rates by Race",
        style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                     fontWeight = 900, textTransform = "uppercase")
      ) %>% 
      hc_subtitle(text = "Select any race to see the trend over time for that race") %>% 
      hc_plotOptions(column = list(grouping = FALSE, shadow = FALSE)) %>% 
      hc_tooltip(valueDecimals = 1,
                 shared = TRUE,
                 split = FALSE,
                 pointFormat= "</b>{point.series.name}: {point.y}% <br>") %>% 
      hc_xAxis(type = "category") %>%
      hc_add_series(
        name = "Completion Rate",
        color = "#bfdcd7",
        pointPadding = -.2,
        data = list(list(name = "American Indian and Alaskan Native",
                         y = {bar_data() %>% 
                             filter(race_long == "American Indian and Alaskan Native") %>% 
                             pull("completion_rate_per_inst_year_race_1")},
                         drilldown = "American Indian and Alaskan Native"),
                    list(name = "Asian",
                         y = {bar_data() %>% 
                             filter(race_long == "Asian") %>% 
                             pull("completion_rate_per_inst_year_race_1")},
                         drilldown = "Asian"),
                    list(name = "Black",
                         y = {bar_data() %>% 
                             filter(race_long == "Black") %>% 
                             pull("completion_rate_per_inst_year_race_1")},
                         drilldown = "Black"),
                    list(name = "Hispanic",
                         y = {bar_data() %>% 
                             filter(race_long == "Hispanic") %>% 
                             pull("completion_rate_per_inst_year_race_1")},
                         drilldown = "Hispanic"),
                    list(name = "Native Hawaiian and Pacific Islander",
                         y = {bar_data() %>% 
                             filter(race_long == "Native Hawaiian and Pacific Islander") %>% 
                             pull("completion_rate_per_inst_year_race_1")},
                         drilldown = "Native Hawaiian and Pacific Islander"),
                    list(name = "Race Unknown",
                         y = {bar_data() %>% 
                             filter(race_long == "Race Unknown") %>% 
                             pull("completion_rate_per_inst_year_race_1")},
                         drilldown = "Race Unknown"),
                    list(name = "Two or More Races",
                         y = {bar_data() %>% 
                             filter(race_long == "Two or More Races") %>% 
                             pull("completion_rate_per_inst_year_race_1")},
                         drilldown = "Two or More Races"),
                    list(name = "White",
                         y = {bar_data() %>% 
                             filter(race_long == "White") %>% 
                             pull("completion_rate_per_inst_year_race_1")},
                         drilldown = "White")
        )
      ) %>% 
      hc_add_series(
        name = "Enrollment Percent",
        color = "#3e6d72",
        pointPadding = 0,
        type = "column",
        data = bar_data()$enrollment_pct_per_inst_year_race_1
      ) %>%
      hc_drilldown(allowPointDrilldown = TRUE,
                   activeAxisLabelStyle = list(text = "does it work?"),
                   series = list(
                     list(
                       name = "AIAN Completion Rate",
                       id= "American Indian and Alaskan Native",
                       color = "black",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "American Indian and Alaskan Native") %>% 
                                             select("year", "completion_rate_per_inst_year_race_2" ))}
                       
                     ),
                     list(name = "AIAN Enrollment Percent",
                          color = "#3e6d72",
                          id= "American Indian and Alaskan Native",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "American Indian and Alaskan Native") %>% 
                                                select("year", "enrollment_pct_per_inst_year_race_2"))}
                     ),
                     list(
                       name = "Asian Completion Rate",
                       id= "Asian",
                       color = "black",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Asian") %>% 
                                             select("year", "completion_rate_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Asian Enrollment Percent",
                          color = "#3e6d72",
                          id= "Asian",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Asian") %>% 
                                                select("year", "enrollment_pct_per_inst_year_race_2"))}
                     ),
                     list(
                       name = "Black Completion Rate",
                       color = "black",
                       id= "Black",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Black") %>% 
                                             select("year", "completion_rate_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Black Enrollment Percent",
                          color = "#3e6d72",
                          id= "Black",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Black") %>% 
                                                select("year", "enrollment_pct_per_inst_year_race_2"))}
                     ),
                     list(
                       name = "Hispanic Completion Rate",
                       color = "black",
                       id= "Hispanic",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Hispanic") %>% 
                                             select("year", "completion_rate_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Hispanic Enrollment Percent",
                          color = "#3e6d72",
                          id= "Hispanic",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Hispanic") %>% 
                                                select("year", "enrollment_pct_per_inst_year_race_2"))}
                     ),
                     list(
                       name = "NHPI Completion Rate",
                       color = "black",
                       id= "Native Hawaiian and Pacific Islander",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Native Hawaiian and Pacific Islander") %>% 
                                             select("year", "completion_rate_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "NHPI Enrollment Percent",
                          color = "#3e6d72",
                          id= "Native Hawaiian and Pacific Islander",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Native Hawaiian and Pacific Islander") %>% 
                                                select("year", "enrollment_pct_per_inst_year_race_2"))}
                     ),
                     list(
                       name = "White Completion Rate",
                       color = "black",
                       id= "White",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "White") %>% 
                                             select("year", "completion_rate_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "White Enrollment Percent",
                          color = "#3e6d72",
                          id= "White",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "White") %>% 
                                                select("year", "enrollment_pct_per_inst_year_race_2"))}
                     ),
                     list(
                       name = "Two or More Races Completion Rate",
                       color = "black",
                       id= "Two or More Races",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Two or More Races") %>% 
                                             select("year", "completion_rate_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Two or More Races Enrollment Percent",
                          color = "#3e6d72",
                          id= "Two or More Races",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Two or More Races") %>% 
                                                select("year", "enrollment_pct_per_inst_year_race_2"))}
                     ),
                     list(
                       name = "Race Unknown Completion Rate",
                       color = "black",
                       id= "Race Unknown",
                       type = "line",
                       data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                             filter(dname == input$dname) %>% 
                                             filter(race_long == "Race Unknown") %>% 
                                             select("year", "completion_rate_per_inst_year_race_2"))}
                       
                     ),
                     list(name = "Race Unknown Enrollment Percent",
                          color = "#3e6d72",
                          id= "Race Unknown",
                          type = "line",
                          data = {list_parse2(efa_fsums_pell_compl_profile_equity %>% 
                                                filter(dname == input$dname) %>% 
                                                filter(race_long == "Race Unknown") %>% 
                                                select("year", "enrollment_pct_per_inst_year_race_2"))}
                     )))
  })


  
  output$retention <- renderHighchart({
    highchart() %>%
      hc_title(text = paste(
                            institution_react()
      ),
      style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                   fontWeight = 900, textTransform = "uppercase")) %>%
      hc_add_series(name = "Full Time Retention",
                    efa_fsums_pell_compl_profile_equity %>%
                      filter(dname == input$dname) %>%
                      select(year, ft_retention) %>% 
                      distinct(),
                    "line", hcaes(x = year, y = ft_retention))%>% 
      hc_add_series(name = "Part Time Retention",
                    efa_fsums_pell_compl_profile_equity %>%
                      filter(dname == input$dname) %>%
                      select(year, pt_retention) %>% 
                      distinct(),
                    "line", hcaes(x = year, y = pt_retention)) %>% 
      #hc_legend(
        # align = "right",
        # verticalAlign = "top",
        # layout = "vertical",
        #title = list(text = '<br/><span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide)</span>')
      #) %>% 
      hc_yAxis(title = list(text = "Retention Rate"),
               min = 0,
               max = 100,
               tickInterval = 20) %>% 
      hc_tooltip(valueDecimals = 0,
                 shared = TRUE,
                 split = FALSE,
                 pointFormat= "</b>{point.series.name}: {point.y}% <br>") %>%
      hc_xAxis(title = list(text = "Year"))
  })
  
  
  
  
  ###### compare trial

  
  output$retention_comp <- renderHighchart({
    validate(
      need(input$dname_comp != "--none--", '  ')
    )
    
    highchart() %>% 
      hc_title(text = paste(
                            comp_institution()
      ),
      style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                   fontWeight = 900, textTransform = "uppercase")) %>% 
      hc_add_series(name = "Full Time Retention",
                    efa_fsums_pell_compl_profile_equity %>%
                      filter(dname == input$dname_comp) %>%
                      select(year, ft_retention) %>% 
                      distinct(),
                    "line", hcaes(x = year, y = ft_retention))%>%
      hc_add_series(name = "Part Time Retention",
                    efa_fsums_pell_compl_profile_equity %>%
                      filter(dname == input$dname_comp) %>% 
                      select(year, pt_retention) %>% 
                      distinct(),
                    "line", hcaes(x = year, y = pt_retention))%>%
      hc_xAxis(title = list(text = "Year")) %>% 
      hc_yAxis(title = list(text = "Retention Rate"),
               min = 0,
               max = 100,
               tickInterval = 20) %>% 
     # hc_legend(
        # align = "right",
        # verticalAlign = "top",
        # layout = "vertical",
        #title = list(text = '<br/><span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide)</span>')
      #) %>% 
      hc_tooltip(valueDecimals = 0,
                 shared = TRUE,
                 split = FALSE,
                 pointFormat= "</b>{point.series.name}: {point.y}% <br>")

  })
  
  
  output$retention_comp2 <- renderHighchart({
    validate(
      need(input$dname2_comp != "--none--", '  ')
    )
    
    highchart() %>% 
      hc_title(text = paste(
                            comp_institution2()
      ),
      style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                   fontWeight = 900, textTransform = "uppercase")) %>% 
      hc_add_series(name = "Full Time Retention",
                    efa_fsums_pell_compl_profile_equity %>%
                      filter(dname == input$dname2_comp) %>%
                      select(year, ft_retention) %>% 
                      distinct(),
                    "line", hcaes(x = year, y = ft_retention))%>%
      hc_add_series(name = "Part Time Retention",
                    efa_fsums_pell_compl_profile_equity %>%
                      filter(dname == input$dname2_comp) %>%
                      select(year, pt_retention) %>% 
                      distinct(),
                    "line", hcaes(x = year, y = pt_retention))%>%
      hc_xAxis(title = list(text = "Year")) %>% 
      hc_yAxis(title = list(text = "Retention Rate"),
               min = 0,
               max = 100,
               tickInterval = 20) %>% 
      # hc_legend(
      # align = "right",
      # verticalAlign = "top",
      # layout = "vertical",
      #title = list(text = '<br/><span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide)</span>')
      #) %>% 
      hc_tooltip(valueDecimals = 0,
                 shared = TRUE,
                 split = FALSE,
                 pointFormat= "</b>{point.series.name}: {point.y}% <br>") 
    
  })
  
  
  
  
  
  output$retention_comp3 <- renderHighchart({
    validate(
      need(input$dname3_comp != "--none--", '  ')
    )
    
    highchart() %>% 
      hc_title(text = paste(
                            comp_institution3()
      ),
      style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                   fontWeight = 900, textTransform = "uppercase")) %>% 
      hc_add_series(name = "Full Time Retention",
                    efa_fsums_pell_compl_profile_equity %>%
                      filter(dname == input$dname3_comp) %>%
                      select(year, ft_retention) %>% 
                      distinct(),
                    "line", hcaes(x = year, y = ft_retention))%>%
      hc_add_series(name = "Part Time Retention",
                    efa_fsums_pell_compl_profile_equity %>%
                      filter(dname == input$dname3_comp) %>%
                      select(year, pt_retention) %>% 
                      distinct(),
                    "line", hcaes(x = year, y = pt_retention))%>%
      hc_xAxis(title = list(text = "Year")) %>% 
      hc_yAxis(title = list(text = "Retention Rate"),
               min = 0,
               max = 100,
               tickInterval = 20) %>% 
      # hc_legend(
      # align = "right",
      # verticalAlign = "top",
      # layout = "vertical",
      #title = list(text = '<br/><span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide)</span>')
      #) %>% 
      hc_tooltip(valueDecimals = 0,
                 shared = TRUE,
                 split = FALSE,
                 pointFormat= "</b>{point.series.name}: {point.y}% <br>") 
    
  })
  
  
  
  
  
 
  #Graduation comparison
  
  graduation_react <- reactive({
    highchart() %>% 
      hc_add_series(name = "American Indian and Alaskan Native",
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      filter(race == "AIAN") %>% 
                      select(year, completers_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = completers_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "Asian",
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      filter(race == "Asian") %>% 
                      select(year, completers_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = completers_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "Black",
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      filter(race == "Black") %>% 
                      select(year, completers_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = completers_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "Hispanic",
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      filter(race == "Hispanic") %>% 
                      select(year, completers_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = completers_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "Native Hawaiian and Pacific Islander",
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      filter(race == "NHPI") %>% 
                      select(year, completers_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = completers_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "Race Unknown",
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      filter(race == "Race Unknown") %>% 
                      select(year, completers_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = completers_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "Two or More Races",
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      filter(race == "Two or More Races") %>% 
                      select(year, completers_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = completers_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "White",
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      filter(race == "White") %>% 
                      select(year, completers_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = completers_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "Overall Graduation Rate",
                    efa_fsums_pell_compl_profile_equity %>% ungroup() %>%
                      select(dname, year,completion_rate_per_inst_year, non_completion_rate_per_inst_year) %>%
                      drop_na %>% 
                      group_by(dname) %>%
                      distinct() %>%
                      pivot_longer(3:4, names_to = "completion", values_to = "value") %>%
                      filter(dname==input$dname) %>% 
                      filter(completion == "completion_rate_per_inst_year") %>% 
                      ungroup()	%>% 
                      select(year, value),
        "line", hcaes(x = year, y = value)
      ) %>% 
      hc_yAxis(title = list(text = "Percent"),
               min = 0,
               max = 1)
  }) 
  
  
  output$graduation <- renderHighchart({
    highchart() %>% 
      hc_title(text = paste(
                            institution_react()
                            ),
               style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                            fontWeight = 900, textTransform = "uppercase")) %>% 
      hc_add_series(name = "Overall Graduation Rate",
                    efa_fsums_pell_compl_profile_equity %>% ungroup() %>%
                      select(dname, year,completion_rate_per_inst_year, non_completion_rate_per_inst_year) %>%
                      drop_na %>% 
                      group_by(dname) %>%
                      distinct() %>%
                      pivot_longer(3:4, names_to = "completion", values_to = "value") %>%
                      filter(dname==input$dname) %>% 
                      filter(completion == "completion_rate_per_inst_year") %>% 
                      ungroup()	%>% 
                      mutate(value = value*100) %>% 
                      select(year, value),
                    "line", hcaes(x = year, y = value)
      ) %>% 
      hc_add_series(name = "American Indian and Alaskan Native",
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      filter(race == "AIAN") %>% 
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race),
                    visible = FALSE) %>% 
      hc_add_series(name = "Asian",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>%
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      filter(race == "Asian") %>% 
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race)) %>% 
      hc_add_series(name = "Black",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      filter(race == "Black") %>% 
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race)) %>% 
      hc_add_series(name = "Hispanic",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      filter(race == "Hispanic") %>% 
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race)) %>% 
      hc_add_series(name = "Native Hawaiian and Pacific Islander",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>%
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      filter(race == "NHPI") %>% 
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race)) %>% 
      hc_add_series(name = "Race Unknown",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      filter(race == "Race Unknown") %>% 
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race)) %>% 
      hc_add_series(name = "Two or More Races",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      filter(race == "Two or More Races") %>% 
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race)) %>% 
      hc_add_series(name = "White",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      filter(race == "White") %>% 
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race)) %>% 
      hc_yAxis(title = list(text = "Percent"),
               min = 0,
               max = 100,
               tickInterval = 20) %>% 
      hc_legend(
        # align = "right",
        # verticalAlign = "top",
        layout = "horizontal",
        title = list(text = 'Races<br/><span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide)</span>')
      ) %>% 
      hc_tooltip(valueDecimals = 0,
                 shared = TRUE,
                 split = FALSE,
                 pointFormat= "</b>{point.series.name}: {point.y}% <br>")
  }) 
  
  comp_institution <- reactive({
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname == input$dname_comp) %>% 
      distinct(dname,ihe_name) %>%
      drop_na %>%
      pull(ihe_name) %>% unique()
  })
  
  comp_institution2 <- reactive({
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname == input$dname2_comp) %>% 
      distinct(dname,ihe_name) %>%
      drop_na %>%
      pull(ihe_name) %>% unique()
  })
  
  comp_institution3 <- reactive({
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname == input$dname3_comp) %>% 
      distinct(dname,ihe_name) %>%
      drop_na %>%
      pull(ihe_name) %>% unique()
  })
  
  output$graduation_comp <- renderHighchart({
    validate(
      need(input$dname_comp != "--none--", '  ')
    )

    
    highchart() %>%
      hc_title(text = paste(
        comp_institution()
      ),
      style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                   fontWeight = 900, textTransform = "uppercase")) %>% 
      hc_add_series(name = "Overall Graduation Rate",
                    efa_fsums_pell_compl_profile_equity %>% ungroup() %>%
                      select(dname, year,completion_rate_per_inst_year, non_completion_rate_per_inst_year) %>%
                      drop_na %>% 
                      group_by(dname) %>%
                      distinct() %>%
                      pivot_longer(3:4, names_to = "completion", values_to = "value") %>%
                      filter(dname==input$dname_comp) %>% 
                      filter(completion == "completion_rate_per_inst_year") %>% 
                      ungroup()	%>% 
                      mutate(value = value*100) %>% 
                      select(year, value),
                    "line", hcaes(x = year, y = value)
      ) %>% 
      hc_add_series(name = "American Indian and Alaskan Native",
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname_comp) %>% 
                      filter(race == "AIAN") %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race),
                    showInLegend = TRUE,
                    visible = FALSE) %>% 
      hc_add_series(name = "Asian",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname_comp) %>% 
                      filter(race == "Asian") %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race),
                    showInLegend = TRUE) %>% 
      hc_add_series(name = "Black",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname_comp) %>% 
                      filter(race == "Black") %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race),
                    showInLegend = TRUE) %>% 
      hc_add_series(name = "Hispanic",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname_comp) %>% 
                      filter(race == "Hispanic") %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race),
                    showInLegend = TRUE) %>% 
      hc_add_series(name = "Native Hawaiian and Pacific Islander",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname_comp) %>% 
                      filter(race == "NHPI") %>%
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race)) %>% 
      hc_add_series(name = "Race Unknown",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname_comp) %>% 
                      filter(race == "Race Unknown") %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race)) %>% 
      hc_add_series(name = "Two or More Races",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname_comp) %>% 
                      filter(race == "Two or More Races") %>%
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race)) %>% 
      hc_add_series(name = "White",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname_comp) %>% 
                      filter(race == "White") %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race)) %>% 
      hc_yAxis(title = list(text = "Percent"),
               min = 0,
               max = 100,
               tickInterval = 20) %>% 
      hc_legend(
        # align = "right",
        #verticalAlign = "right",
        # layout = "vertical",
        title = list(text = 'Races<br/><span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide)</span>')
      ) %>% 
      hc_tooltip(valueDecimals = 0,
                 shared = TRUE,
                 split = FALSE,
                 pointFormat= "</b>{point.series.name}: {point.y}% <br>")
    
  
  })
  
  
  
  output$graduation_comp2 <- renderHighchart({
    validate(
      need(input$dname2_comp != "--none--", '  ')
    )
    
    highchart() %>%
      hc_title(text = paste(
        comp_institution2()
      ),
      style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                   fontWeight = 900, textTransform = "uppercase")) %>% 
      hc_add_series(name = "Overall Graduation Rate",
                    efa_fsums_pell_compl_profile_equity %>% ungroup() %>%
                      select(dname, year,completion_rate_per_inst_year, non_completion_rate_per_inst_year) %>%
                      drop_na %>% 
                      group_by(dname) %>%
                      distinct() %>%
                      pivot_longer(3:4, names_to = "completion", values_to = "value") %>%
                      filter(dname==input$dname2_comp) %>% 
                      filter(completion == "completion_rate_per_inst_year") %>% 
                      ungroup()	%>% 
                      mutate(value = value*100) %>% 
                      select(year, value),
                    "line", hcaes(x = year, y = value)
      ) %>% 
      hc_add_series(name = "American Indian and Alaskan Native",
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname2_comp) %>% 
                      filter(race == "AIAN") %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race),
                    showInLegend = TRUE,
                    visible = FALSE) %>% 
      hc_add_series(name = "Asian",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname2_comp) %>% 
                      filter(race == "Asian") %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race),
                    showInLegend = TRUE) %>% 
      hc_add_series(name = "Black",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname2_comp) %>% 
                      filter(race == "Black") %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race),
                    showInLegend = TRUE) %>% 
      hc_add_series(name = "Hispanic",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname2_comp) %>% 
                      filter(race == "Hispanic") %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race),
                    showInLegend = TRUE) %>% 
      hc_add_series(name = "Native Hawaiian and Pacific Islander",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname2_comp) %>% 
                      filter(race == "NHPI") %>%
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race)) %>% 
      hc_add_series(name = "Race Unknown",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname2_comp) %>% 
                      filter(race == "Race Unknown") %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race)) %>% 
      hc_add_series(name = "Two or More Races",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname2_comp) %>% 
                      filter(race == "Two or More Races") %>%
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race)) %>% 
      hc_add_series(name = "White",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname2_comp) %>% 
                      filter(race == "White") %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race)) %>% 
      hc_yAxis(title = list(text = "Percent"),
               min = 0,
               max = 100,
               tickInterval = 20) %>% 
      hc_legend(
        # align = "right",
        #verticalAlign = "right",
        # layout = "vertical",
        title = list(text = 'Races<br/><span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide)</span>')
      ) %>% 
      hc_tooltip(valueDecimals = 0,
                 shared = TRUE,
                 split = FALSE,
                 pointFormat= "</b>{point.series.name}: {point.y}% <br>")
    
    
  })
  
  
  
  output$graduation_comp3 <- renderHighchart({
    validate(
      need(input$dname3_comp != "--none--", '  ')
    )
  
    
    highchart() %>%
      hc_title(text = paste(
        comp_institution3()
      ),
      style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                   fontWeight = 900, textTransform = "uppercase")) %>% 
      hc_add_series(name = "Overall Graduation Rate",
                    efa_fsums_pell_compl_profile_equity %>% ungroup() %>%
                      select(dname, year,completion_rate_per_inst_year, non_completion_rate_per_inst_year) %>%
                      drop_na %>% 
                      group_by(dname) %>%
                      distinct() %>%
                      pivot_longer(3:4, names_to = "completion", values_to = "value") %>%
                      filter(dname==input$dname3_comp) %>% 
                      filter(completion == "completion_rate_per_inst_year") %>% 
                      ungroup()	%>% 
                      mutate(value = value*100) %>% 
                      select(year, value),
                    "line", hcaes(x = year, y = value)
      ) %>% 
      hc_add_series(name = "American Indian and Alaskan Native",
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname3_comp) %>% 
                      filter(race == "AIAN") %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race),
                    showInLegend = TRUE,
                    visible = FALSE) %>% 
      hc_add_series(name = "Asian",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname3_comp) %>% 
                      filter(race == "Asian") %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race),
                    showInLegend = TRUE) %>% 
      hc_add_series(name = "Black",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname3_comp) %>% 
                      filter(race == "Black") %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race),
                    showInLegend = TRUE) %>% 
      hc_add_series(name = "Hispanic",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname3_comp) %>% 
                      filter(race == "Hispanic") %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race),
                    showInLegend = TRUE) %>% 
      hc_add_series(name = "Native Hawaiian and Pacific Islander",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname3_comp) %>% 
                      filter(race == "NHPI") %>%
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race)) %>% 
      hc_add_series(name = "Race Unknown",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname3_comp) %>% 
                      filter(race == "Race Unknown") %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race)) %>% 
      hc_add_series(name = "Two or More Races",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname3_comp) %>% 
                      filter(race == "Two or More Races") %>%
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race)) %>% 
      hc_add_series(name = "White",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname3_comp) %>% 
                      filter(race == "White") %>% 
                      mutate(completion_rate_per_inst_year_race = round(completion_rate_per_inst_year_race*100, 0)) %>%
                      select(year, completion_rate_per_inst_year_race),
                    "line", hcaes(x = year, y = completion_rate_per_inst_year_race)) %>% 
      hc_yAxis(title = list(text = "Percent"),
               min = 0,
               max = 100,
               tickInterval = 20) %>% 
      hc_legend(
        # align = "right",
        #verticalAlign = "right",
        # layout = "vertical",
        title = list(text = 'Races<br/><span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide)</span>')
      ) %>% 
      hc_tooltip(valueDecimals = 0,
                 shared = TRUE,
                 split = FALSE,
                 pointFormat= "</b>{point.series.name}: {point.y}% <br>")
    
    
  })
  
  
  output$enrollment <- renderHighchart({
    highchart() %>% 
      hc_title(text = paste(
        institution_react()
      ),
      style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                   fontWeight = 900, textTransform = "uppercase")) %>% 
      hc_add_series(name = "American Indian and Alaskan Native",
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      filter(race == "AIAN") %>% 
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "Asian",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>%
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      filter(race == "Asian") %>% 
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "Black",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      filter(race == "Black") %>% 
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "Hispanic",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      filter(race == "Hispanic") %>% 
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "Native Hawaiian and Pacific Islander",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>%
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      filter(race == "NHPI") %>% 
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "Race Unknown",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      filter(race == "Race Unknown") %>% 
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "Two or More Races",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      filter(race == "Two or More Races") %>% 
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "White",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname) %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      filter(race == "White") %>% 
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_yAxis(title = list(text = "Percent"),
               min = 0,
               max = 100,
               tickInterval = 20) %>% 
      hc_legend(
        # align = "right",
        # verticalAlign = "top",
        layout = "horizontal",
        title = list(text = 'Races<br/><span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide)</span>')
      ) %>% 
      hc_tooltip(valueDecimals = 0,
                 shared = TRUE,
                 split = FALSE,
                 pointFormat= "</b>{point.series.name}: {point.y}% <br>")
  }) 
  
  
  output$enrollment_comp <- renderHighchart({
    validate(
      need(input$dname_comp != "--none--", '  ')
    )
    
   # base.plot<- graduation_react()
    
    highchart() %>%
      hc_title(text = paste(
        comp_institution()
      ),
      style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                   fontWeight = 900, textTransform = "uppercase")) %>% 
      hc_add_series(name = "American Indian and Alaskan Native",
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname_comp) %>% 
                      filter(race == "AIAN") %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race),
                    showInLegend = TRUE) %>% 
      hc_add_series(name = "Asian",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname_comp) %>% 
                      filter(race == "Asian") %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race),
                    showInLegend = TRUE) %>% 
      hc_add_series(name = "Black",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname_comp) %>% 
                      filter(race == "Black") %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race),
                    showInLegend = TRUE) %>% 
      hc_add_series(name = "Hispanic",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname_comp) %>% 
                      filter(race == "Hispanic") %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race),
                    showInLegend = TRUE) %>% 
      hc_add_series(name = "Native Hawaiian and Pacific Islander",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname_comp) %>% 
                      filter(race == "NHPI") %>%
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "Race Unknown",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname_comp) %>% 
                      filter(race == "Race Unknown") %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "Two or More Races",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname_comp) %>% 
                      filter(race == "Two or More Races") %>%
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "White",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname_comp) %>% 
                      filter(race == "White") %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_yAxis(title = list(text = "Percent"),
               min = 0,
               max = 100,
               tickInterval = 20) %>% 
      hc_legend(
        # align = "right",
        #verticalAlign = "right",
        # layout = "vertical",
        title = list(text = 'Races<br/><span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide)</span>')
      ) %>% 
      hc_tooltip(valueDecimals = 0,
                 shared = TRUE,
                 split = FALSE,
                 pointFormat= "</b>{point.series.name}: {point.y}% <br>")
    
    
  })
  
  output$enrollment_comp2 <- renderHighchart({
    validate(
      need(input$dname2_comp != "--none--", '  ')
    )
    
    highchart() %>%
      hc_title(text = paste(
        comp_institution2()
      ),
      style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                   fontWeight = 900, textTransform = "uppercase")) %>% 
      hc_add_series(name = "American Indian and Alaskan Native",
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname2_comp) %>% 
                      filter(race == "AIAN") %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race),
                    showInLegend = TRUE,
                    visible = TRUE) %>% 
      hc_add_series(name = "Asian",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname2_comp) %>% 
                      filter(race == "Asian") %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race),
                    showInLegend = TRUE) %>% 
      hc_add_series(name = "Black",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname2_comp) %>% 
                      filter(race == "Black") %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race),
                    showInLegend = TRUE) %>% 
      hc_add_series(name = "Hispanic",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname2_comp) %>% 
                      filter(race == "Hispanic") %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race),
                    showInLegend = TRUE) %>% 
      hc_add_series(name = "Native Hawaiian and Pacific Islander",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname2_comp) %>% 
                      filter(race == "NHPI") %>%
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "Race Unknown",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname2_comp) %>% 
                      filter(race == "Race Unknown") %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "Two or More Races",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname2_comp) %>% 
                      filter(race == "Two or More Races") %>%
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "White",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname2_comp) %>% 
                      filter(race == "White") %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_yAxis(title = list(text = "Percent"),
               min = 0,
               max = 100,
               tickInterval = 20) %>% 
      hc_legend(
        # align = "right",
        #verticalAlign = "right",
        # layout = "vertical",
        title = list(text = 'Races<br/><span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide)</span>')
      ) %>% 
      hc_tooltip(valueDecimals = 0,
                 shared = TRUE,
                 split = FALSE,
                 pointFormat= "</b>{point.series.name}: {point.y}% <br>")
    
    
  })
  
  output$enrollment_comp3 <- renderHighchart({
    validate(
      need(input$dname3_comp != "--none--", '  ')
    )
    
    highchart() %>%
      hc_title(text = paste(
        comp_institution3()
      ),
      style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                   fontWeight = 900, textTransform = "uppercase")) %>% 
      hc_add_series(name = "American Indian and Alaskan Native",
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname3_comp) %>% 
                      filter(race == "AIAN") %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race),
                    showInLegend = TRUE,
                    visible = TRUE) %>% 
      hc_add_series(name = "Asian",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname3_comp) %>% 
                      filter(race == "Asian") %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race),
                    showInLegend = TRUE) %>% 
      hc_add_series(name = "Black",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname3_comp) %>% 
                      filter(race == "Black") %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race),
                    showInLegend = TRUE) %>% 
      hc_add_series(name = "Hispanic",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname3_comp) %>% 
                      filter(race == "Hispanic") %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race),
                    showInLegend = TRUE) %>% 
      hc_add_series(name = "Native Hawaiian and Pacific Islander",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname3_comp) %>% 
                      filter(race == "NHPI") %>%
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "Race Unknown",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname3_comp) %>% 
                      filter(race == "Race Unknown") %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "Two or More Races",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname3_comp) %>% 
                      filter(race == "Two or More Races") %>%
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_add_series(name = "White",
                    visible = FALSE,
                    efa_fsums_pell_compl_profile_equity %>% 
                      filter(dname == input$dname3_comp) %>% 
                      filter(race == "White") %>% 
                      mutate(enrollment_pct_per_inst_year_race = round(enrollment_pct_per_inst_year_race*100, 0)) %>%
                      select(year, enrollment_pct_per_inst_year_race),
                    "line", hcaes(x = year, y = enrollment_pct_per_inst_year_race)) %>% 
      hc_yAxis(title = list(text = "Percent"),
               min = 0,
               max = 100,
               tickInterval = 20) %>% 
      hc_legend(
        # align = "right",
        #verticalAlign = "right",
        # layout = "vertical",
        title = list(text = 'Races<br/><span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide)</span>')
      ) %>% 
      hc_tooltip(valueDecimals = 0,
                 shared = TRUE,
                 split = FALSE,
                 pointFormat= "</b>{point.series.name}: {point.y}% <br>")
    
    
  })
  
  
  # transfer ----.
  output$transfer <- renderHighchart({
    highchart() %>%
      hc_title(text = paste(
                            institution_react()
      ),
      style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                   fontWeight = 900, textTransform = "uppercase")) %>%
      hc_add_series(name = "Transfer-Out Rate",
                    efa_fsums_pell_compl_profile_equity %>%
                      filter(dname == input$dname) %>%
                      select(year, transfer_out_rate) %>% 
                      distinct(),
                    "line", hcaes(x = year, y = transfer_out_rate))%>% 
      #hc_legend(
      # align = "right",
      # verticalAlign = "top",
      # layout = "vertical",
      #title = list(text = '<br/><span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide)</span>')
      #) %>% 
      hc_legend(enabled = FALSE) %>% 
      hc_yAxis(title = list(text = "Transfer-Out Rate"),
               min = 0,
               max = 100,
               tickInterval = 20) %>% 
      hc_tooltip(valueDecimals = 0,
                 shared = TRUE,
                 split = FALSE,
                 pointFormat= "</b>{point.series.name}: {point.y}% <br>") %>%
      hc_xAxis(title = list(text = "Year")) 
  })
  
  
    output$transfer_comp <- renderHighchart({
  validate(
    need(input$dname_comp != "--none--", '  ')
  )
  
  highchart() %>% 
    hc_title(text = paste(
      comp_institution()
    ),
    style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                 fontWeight = 900, textTransform = "uppercase")) %>% 
    hc_add_series(name = "Transfer-Out Rate",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname_comp) %>%
                    select(year, transfer_out_rate) %>% 
                    distinct(),
                  "line", hcaes(x = year, y = transfer_out_rate))%>%
    hc_xAxis(title = list(text = "Year")) %>% 
    hc_yAxis(title = list(text = "Transfer-Out Rate"),
             min = 0,
             max = 100,
             tickInterval = 20) %>% 
    hc_legend(enabled = FALSE) %>% 
    hc_tooltip(valueDecimals = 0,
               shared = TRUE,
               split = FALSE,
               pointFormat= "</b>{point.series.name}: {point.y}% <br>")
  
})


output$transfer_comp2 <- renderHighchart({
  validate(
    need(input$dname2_comp != "--none--", '  ')
  )
  
  highchart() %>% 
    hc_title(text = paste(
      comp_institution2()
    ),
    style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                 fontWeight = 900, textTransform = "uppercase")) %>% 
    hc_add_series(name = "Transfer-Out Rate",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname2_comp) %>%
                    select(year, transfer_out_rate) %>% 
                    distinct(),
                  "line", hcaes(x = year, y = transfer_out_rate))%>%
    hc_xAxis(title = list(text = "Year")) %>% 
    hc_yAxis(title = list(text = "Transfer-Out Rate"),
             min = 0,
             max = 100,
             tickInterval = 20) %>% 
    hc_legend(enabled = FALSE) %>% 
    hc_tooltip(valueDecimals = 0,
               shared = TRUE,
               split = FALSE,
               pointFormat= "</b>{point.series.name}: {point.y}% <br>") 
  
})





output$transfer_comp3 <- renderHighchart({
  validate(
    need(input$dname3_comp != "--none--", '  ')
  )
  
  highchart() %>% 
    hc_title(text = paste(
      comp_institution3()
    ),
    style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                 fontWeight = 900, textTransform = "uppercase")) %>% 
    hc_add_series(name = "Transfer-Out Rate",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname3_comp) %>% 
                    select(year, transfer_out_rate) %>% 
                    distinct(),
                  "line", hcaes(x = year, y = transfer_out_rate))%>%
    hc_xAxis(title = list(text = "Year")) %>% 
    hc_yAxis(title = list(text = "Transfer-Out Rate"),
             min = 0,
             max = 100,
             tickInterval = 20) %>% 
    hc_legend(enabled = FALSE) %>% 
    hc_tooltip(valueDecimals = 0,
               shared = TRUE,
               split = FALSE,
               pointFormat= "</b>{point.series.name}: {point.y}% <br>") 
  
})
  
  



# Outcomes stacked bar -----.

output$outcomes <- renderHighchart({
  
  fnOut <- "function(){
          const $this = this;
          return $this.stack;
        }"
  
  highchart() %>% 
    hc_title(text = paste(
      institution_react()
    ),
    style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                 fontWeight = 900, textTransform = "uppercase")) %>%
    hc_chart(type = "column") %>% 
    hc_plotOptions(column = list(stacking = "normal")) %>% 
    hc_xAxis(categories = efa_fsums_pell_compl_profile_equity %>%
               filter(dname == input$dname) %>%
               filter(year == max(year)) %>% 
               select(cohort, pct_enrolled_unknown_pell) %>% 
               distinct() %>% 
               pull(cohort)) %>% 
    hc_add_series(name = "Percent Enrollment Unknown",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname) %>%
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_unknown_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_unknown_pell),
                  stack = "Pell",
                  color = "#3e6d72",
                  showInLegend = FALSE,
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE,
                                      color = "white"
                                    ))) %>% 
    hc_add_series(name = "Percent enrolled subsequently at another institution",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname) %>% 
                    filter(year == max(year)) %>%  
                    select(cohort, pct_enrolled_other_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_other_pell),
                  stack = "Pell",
                  color = "gray",
                  showInLegend = FALSE,
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE,
                                      color = "white"
                                    ))) %>% 
    hc_add_series(name = "Percent still enrolled at Institution",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_pell),
                  stack = "Pell",
                  color = "black",
                  showInLegend = FALSE,
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE,
                                      color = "white"
                                    ))) %>% 
    hc_add_series(name = "Percent receiving degree or certificate",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname) %>% 
                    filter(year == max(year)) %>%  
                    select(cohort, pct_award_pell) %>% 
                    distinct() %>% 
                    pull(pct_award_pell),
                  stack = "Pell",
                  color = "#bfdcd7",
                  showInLegend = FALSE,
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE,
                                      color = "black"
                                    ))) %>% 
    hc_add_series(name = "Percent Enrollment Unknown",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_unknown_non_pell) %>% 
                    distinct() %>%
                    pull(pct_enrolled_unknown_non_pell),
                  stack = "NonPell",
                  color = "#3e6d72",
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE,
                                      color = "white"
                                    ))) %>%
    hc_add_series(name = "Percent enrolled subsequently at another institution",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_other_non_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_other_non_pell),
                  stack = "NonPell",
                  color = "gray",
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE,
                                      color = "white"
                                    ))) %>%
    hc_add_series(name = "Percent still enrolled at Institution",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_non_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_non_pell),
                  stack = "NonPell",
                  color = "black",
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE,
                                      color = "white"
                                    ))) %>%
    hc_add_series(name = "Percent receiving degree or certificate",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_award_non_pell) %>% 
                    distinct() %>% 
                    pull(pct_award_non_pell),
                  stack = "NonPell",
                  color = "#bfdcd7",
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE,
                                      color = "black"
                                    ))) %>% 
    hc_yAxis(stackLabels = list(enabled = TRUE,
                                formatter = JS(fnOut))) 
})

  
  

# -----. Outcomes stacked bar comparison 1

output$outcomes_comp <- renderHighchart({
  
  fnOut <- "function(){
          const $this = this;
          return $this.stack;
        }"
  
  validate(
    need(input$dname_comp != "--none--", '  ')
  )
  

  highchart() %>% 
    hc_title(text = paste(
      comp_institution()
    ),
    style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                 fontWeight = 900, textTransform = "uppercase")) %>% 
    hc_chart(type = "column") %>% 
    hc_plotOptions(column = list(stacking = "normal")) %>% 
    hc_xAxis(categories = efa_fsums_pell_compl_profile_equity %>%
               filter(dname == input$dname) %>%
               filter(year == max(year)) %>% 
               select(cohort, pct_enrolled_unknown_pell) %>% 
               distinct() %>% 
               pull(cohort)) %>%  
    hc_add_series(name = "Percent Enrollment Unknown",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_unknown_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_unknown_pell),
                  stack = "Pell",
                  color = "#3e6d72",
                  showInLegend = FALSE,
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE
                                    ))) %>% 
    hc_add_series(name = "Percent enrolled subsequently at another institution",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_other_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_other_pell),
                  stack = "Pell",
                  color = "gray",
                  showInLegend = FALSE,
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE
                                    ))) %>% 
    hc_add_series(name = "Percent still enrolled at Institution",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_pell),
                  stack = "Pell",
                  color = "black",
                  showInLegend = FALSE,
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE
                                    ))) %>% 
    hc_add_series(name = "Percent receiving degree or certificate",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_award_pell) %>% 
                    distinct() %>% 
                    pull(pct_award_pell),
                  stack = "Pell",
                  color = "#bfdcd7",
                  showInLegend = FALSE,
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE,
                                      color = "black"
                                    ))) %>% 
    hc_add_series(name = "Percent Enrollment Unknown",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_unknown_non_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_unknown_non_pell),
                  stack = "NonPell",
                  color = "#3e6d72",
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE
                                    ))) %>%
    hc_add_series(name = "Percent enrolled subsequently at another institution",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_other_non_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_other_non_pell),
                  stack = "NonPell",
                  color = "gray",
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE
                                    ))) %>%
    hc_add_series(name = "Percent still enrolled at Institution",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_non_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_non_pell),
                  stack = "NonPell",
                  color = "black",
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE
                                    ))) %>%
    hc_add_series(name = "Percent receiving degree or certificate",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_award_non_pell) %>% 
                    distinct() %>% 
                    pull(pct_award_non_pell),
                  stack = "NonPell",
                  color = "#bfdcd7",
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE,
                                      color = "black"
                                    ))) %>% 
    hc_yAxis(stackLabels = list(enabled = TRUE,
                                formatter = JS(fnOut))) 
  
})





# -----. Outcomes stacked bar comparison 2

output$outcomes_comp2 <- renderHighchart({
  
  fnOut <- "function(){
          const $this = this;
          return $this.stack;
        }"
  
  validate(
    need(input$dname2_comp != "--none--", '  ')
  )
  
  highchart() %>% 
    hc_title(text = paste(
      comp_institution2()
    ),
    style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                 fontWeight = 900, textTransform = "uppercase")) %>% 
    hc_chart(type = "column") %>% 
    hc_plotOptions(column = list(stacking = "normal")) %>% 
    hc_xAxis(categories = efa_fsums_pell_compl_profile_equity %>%
               filter(dname == input$dname) %>%
               filter(year == max(year)) %>% 
               select(cohort, pct_enrolled_unknown_pell) %>% 
               distinct() %>% 
               pull(cohort)) %>% 
    hc_add_series(name = "Percent Enrollment Unknown",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname2_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_unknown_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_unknown_pell),
                  stack = "Pell",
                  color = "#3e6d72",
                  showInLegend = FALSE,
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE
                                    ))) %>% 
    hc_add_series(name = "Percent enrolled subsequently at another institution",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname2_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_other_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_other_pell),
                  stack = "Pell",
                  color = "gray",
                  showInLegend = FALSE,
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE
                                    ))) %>% 
    hc_add_series(name = "Percent still enrolled at Institution",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname2_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_pell),
                  stack = "Pell",
                  color = "black",
                  showInLegend = FALSE,
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE
                                    ))) %>% 
    hc_add_series(name = "Percent receiving degree or certificate",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname2_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_award_pell) %>% 
                    distinct() %>% 
                    pull(pct_award_pell),
                  stack = "Pell",
                  color = "#bfdcd7",
                  showInLegend = FALSE,
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE,
                                      color = "black"
                                    ))) %>% 
    hc_add_series(name = "Percent Enrollment Unknown",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname2_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_unknown_non_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_unknown_non_pell),
                  stack = "NonPell",
                  color = "#3e6d72",
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE
                                    ))) %>%
    hc_add_series(name = "Percent enrolled subsequently at another institution",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname2_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_other_non_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_other_non_pell),
                  stack = "NonPell",
                  color = "gray",
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE
                                    ))) %>%
    hc_add_series(name = "Percent still enrolled at Institution",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname2_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_non_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_non_pell),
                  stack = "NonPell",
                  color = "black",
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE
                                    ))) %>%
    hc_add_series(name = "Percent receiving degree or certificate",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname2_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_award_non_pell) %>% 
                    distinct() %>% 
                    pull(pct_award_non_pell),
                  stack = "NonPell",
                  color = "#bfdcd7",
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE,
                                      color = "black"
                                    ))) %>% 
    hc_yAxis(stackLabels = list(enabled = TRUE,
                                formatter = JS(fnOut))) 
})





# -----. Outcomes stacked bar comparison 3

output$outcomes_comp3 <- renderHighchart({
  
  fnOut <- "function(){
          const $this = this;
          return $this.stack;
        }"
  
  validate(
    need(input$dname3_comp != "--none--", '  ')
  )
  
  
  
  highchart() %>% 
    hc_title(text = paste(
      comp_institution3()
    ),
    style = list(fontSize = "18px", color = "black", fontFamily = "Roboto",
                 fontWeight = 900, textTransform = "uppercase")) %>%
    hc_chart(type = "column") %>% 
    hc_plotOptions(column = list(stacking = "normal")) %>% 
    hc_xAxis(categories = efa_fsums_pell_compl_profile_equity %>%
               filter(dname == input$dname) %>%
               filter(year == max(year)) %>% 
               select(cohort, pct_enrolled_unknown_pell) %>% 
               distinct() %>% 
               pull(cohort)) %>% 
    hc_add_series(name = "Percent Enrollment Unknown",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname3_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_unknown_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_unknown_pell),
                  stack = "Pell",
                  color = "#3e6d72",
                  showInLegend = FALSE,
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE,
                                      color = "white"
                                    ))) %>% 
    hc_add_series(name = "Percent enrolled subsequently at another institution",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname3_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_other_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_other_pell),
                  stack = "Pell",
                  color = "gray",
                  showInLegend = FALSE,
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE,
                                      color = "white"
                                    ))) %>% 
    hc_add_series(name = "Percent still enrolled at Institution",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname3_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_pell),
                  stack = "Pell",
                  color = "black",
                  showInLegend = FALSE,
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE,
                                      color = "white"
                                    ))) %>% 
    hc_add_series(name = "Percent receiving degree or certificate",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname3_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_award_pell) %>% 
                    distinct() %>% 
                    pull(pct_award_pell),
                  stack = "Pell",
                  color = "#bfdcd7",
                  showInLegend = FALSE,
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE,
                                      color = "black"
                                    ))) %>% 
    hc_add_series(name = "Percent Enrollment Unknown",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname3_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_unknown_non_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_unknown_non_pell),
                  stack = "NonPell",
                  color = "#3e6d72",
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE,
                                      color = "white"
                                    ))) %>%
    hc_add_series(name = "Percent enrolled subsequently at another institution",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname3_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_other_non_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_other_non_pell),
                  stack = "NonPell",
                  color = "gray",
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE,
                                      color = "white"
                                    ))) %>%
    hc_add_series(name = "Percent still enrolled at Institution",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname3_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_enrolled_non_pell) %>% 
                    distinct() %>% 
                    pull(pct_enrolled_non_pell),
                  stack = "NonPell",
                  color = "black",
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE
                                    ))) %>%
    hc_add_series(name = "Percent receiving degree or certificate",
                  efa_fsums_pell_compl_profile_equity %>%
                    filter(dname == input$dname3_comp) %>% 
                    filter(year == max(year)) %>% 
                    select(cohort, pct_award_non_pell) %>% 
                    distinct() %>% 
                    pull(pct_award_non_pell),
                  stack = "NonPell",
                  color = "#bfdcd7",
                  dataLabels = list(enabled = TRUE,
                                    style = list(
                                      textOutline = FALSE,
                                      color = "black"
                                    ))) %>% 
    hc_yAxis(stackLabels = list(enabled = TRUE,
                                formatter = JS(fnOut)))
})



  
  ## Profile Tab ----
  
  # Generic reactive for profile
  profile_react<-reactive({
    
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname == input$dname)
    
  })
  
  sector_choice<-reactive({
    fsums %>%
      filter(dname %in% input$dname) %>%
      distinct(sector) %>%
      pull(sector) %>% 
      unique
  })
  
  institution_react <- reactive ({
    profile_react() %>%
      distinct(dname,ihe_name) %>%
      drop_na %>%
      pull(ihe_name) %>% unique()
  })
  

  # Reactive for profile
  #  data_profile<-reactive({
  
  #    validate(
  #      need(input$sector, 'Please select sector, state, and institution')
  #    )
  
  #    profile_react() %>% 
  #      pull(ihe_name)
  #  })
  
  # output$ihe_name <- renderUI({
  #   if(is.null(input$dname)) return()
  #   
  #   profile_react() %>%
  #     distinct(dname,ihe_name) %>%
  #     drop_na %>%
  #     pull(ihe_name) %>% unique()
  # })
  
  name_react <- reactive({
    efa_fsums_pell_compl_profile_equity %>%
      filter(dname == input$dname) %>% 
      distinct(dname,ihe_name) %>%
      drop_na %>%
      pull(ihe_name) %>% unique()
  })
  
  output$name_icon <- renderText({
    ifelse(input$dname == "--none--",  paste(" "), paste(as.character(icon("landmark", lib = "font-awesome")), name_react()))
  })
  
  # output$ihe_location <- renderUI({
  #   if(is.null(input$dname)) return()
  #   
  #   profile_react() %>%
  #     distinct(dname,year,location) %>%
  #     filter(year == 2019) %>% 
  #     #filter(year==max(year)) %>%
  #     pull(location) %>% unique()
  #   
  #   #HTML(glue("<font-size: 10px;>Location: {location}</font>"))
  # })
  
  location_react <- reactive({
    
    efa_fsums_pell_compl_profile_equity %>%
      filter(year== max(year)) %>%
      filter(dname == input$dname) %>% 
      distinct(dname,year,location) %>%
      #filter(year == 2019) %>%
      pull(location) %>% unique()
    
    
  })
  
  output$location_icon <- renderText ({
    ifelse(input$dname == "--none--", paste(" "), paste(as.character(icon("map-marker", lib = "glyphicon")), location_react()))
    
  })
  
  output$tribal <- renderUI({
    if(is.null(input$dname)) return()
    
    efa_fsums_pell_compl_profile_equity %>% 
      filter(year == max(year)) %>% 
      filter(dname == input$dname) %>% 
      pull(tribal) %>% unique()
  })
  
  
  # output$hbcu <- renderUI({
  #   if(is.null(input$dname)) return()
  #   
  #   efa_fsums_pell_compl_profile_equity %>% 
  #     filter(dname == input$dname) %>% 
  #     pull(hbcu) %>% unique()
  # })
  
  hbcu_react <- reactive({
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname == input$dname) %>% 
      filter(year == max(year)) %>% 
      pull(hbcu) %>% unique()
  })
  
  tribal_react <- reactive({
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname == input$dname) %>% 
      filter(year == max(year)) %>% 
      pull(tribal) %>% unique()
  })
  
  output$hbcu <- renderText({
    if(is.null(input$dname)) return()
    
    if(hbcu_react() == "Historically Black College or University"){
      condition <- T
    }else{
      condition <- F
    }
    ifelse(condition,
           paste(as.character(icon("graduation-cap")), hbcu_react()), paste(hbcu_react()))
  })
  
  output$tribal <- renderText({
    if(is.null(input$dname)) return()
    
    if(tribal_react() == "Tribal College"){
      condition <- T
    }else{
      condition <- F
    }
    ifelse(condition,
           paste(as.character(icon("graduation-cap")), tribal_react()), paste(tribal_react()))
  })
  
  land_grant_react <- reactive({
    if(is.null(input$dname)) return()
    
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname == input$dname) %>% 
      pull(landgrnt) %>% unique()
  })
  
  output$land_grant <- renderText({
    if(is.null(input$dname)) return()
    
    if(land_grant_react() == "Land Grant Institution"){
      condition <- T
    }else{
      condition <- F
    }
    ifelse(condition,
           paste(as.character(icon("seedling")), land_grant_react()), paste(land_grant_react()))
      })

  

  
  location_stuff <- renderText({
    if(is.null(input$dname)) return()
    
    if(land_grant_react() == "Land Grant Institution"){
      condition <- T
    }else{
      condition <- F
    }
    ifelse(condition,
           paste(as.character(icon("seedling")), location_react()), paste(location_react()))
  })
  
  
  dstnugp_react <- reactive({
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname == input$dname) %>%  
      filter(year == max(year)) %>% 
      drop_na(dstnugp) %>%
      pull(dstnugp) %>% unique()
  })
  
  output$dstnugp <- renderText({
    paste(dstnugp_react())
  })
  
  stusrv8_react <- reactive({
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname == input$dname) %>% 
      filter(year == max(year)) %>% 
      drop_na(stusrv8) %>%
      pull(stusrv8) %>%  unique()
  })
  
  output$stusrv8 <- renderText({
    paste(stusrv8_react())
  })
  
  stusrv4_react <- reactive({
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname == input$dname) %>%  
      filter(year == max(year)) %>%
      drop_na(stusrv4) %>% 
      pull(stusrv4) %>%  unique()
  })
  
  output$stusrv4 <- renderText({
    paste(stusrv4_react())
  })
  
  stusrv3_react <- reactive({
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname == input$dname) %>%  
      filter(year == max(year)) %>% 
      drop_na(stusrv3) %>%
      pull(stusrv3) %>%  unique()
  })
  
  output$stusrv3 <- renderText({
    paste(stusrv3_react())
  })
  
  stusrv2_react <- reactive({
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname == input$dname) %>%  
      filter(year == max(year)) %>% 
      drop_na(stusrv2) %>%
      pull(stusrv2) %>%  unique()
  })
  
  output$stusrv2 <- renderText({
    paste(stusrv2_react())
  })
  
  stusrv1_react <- reactive({
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname == input$dname) %>%  
      #filter(year == max(year)) %>% 
      filter(year == 2021) %>% #stusrv1 wasn't in the 2022 survey for some reason...So we are going with 2021
      drop_na(stusrv1) %>%
      pull(stusrv1) %>%  unique()
  })
  
  output$stusrv1 <- renderText({
    paste(stusrv1_react())
  })
  
  slo7_react <- reactive({
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname == input$dname) %>%  
      filter(year == max(year)) %>% 
      drop_na(slo7) %>%
      pull(slo7) %>%  unique()
  })
  
  output$slo7 <- renderText({
    paste(slo7_react())
  })
  
  credits2_react <- reactive({
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname == input$dname) %>%  
      filter(year == max(year)) %>% 
      drop_na(credits2) %>%
      pull(credits2) %>%  unique()
  })
  
  output$credits2 <- renderText({
    paste(credits2_react())
  })
  
  
  
  # -----. fsums generic reactive
  fsums <- fsums %>% 
    mutate(percentile = parse_number(pct_rank_pct)) %>% 
    separate(pct_rank, 
             into = c("num", "char"), 
             sep = "(?<=[0-9])(?=[A-Za-z])",
             remove = FALSE
    )
  
  fsums_react<-reactive({
    
    fsums %>%
      filter(dname == input$dname)
  })
  
  # FHI score
  
  output$ihe_fhiscore <- renderUI({
    if(is.null(input$dname)) return()
    
    ihe_fhiscore<- fsums_react() %>%
      distinct(dname, score_total) %>%
      pull(score_total) %>% unique()
    
    HTML(glue("{ihe_fhiscore}"))
  })
  
  output$ihe_percentile <- renderText({
    if(is.null(input$dname)) return()
    
    fsums_react() %>%
      distinct(dname, pct_rank_pct) %>%
      pull(pct_rank_pct) %>% unique()
    
    #HTML(glue("<b>Fulltime Enrollment (undergrad | grad)</b>
    #          <font size = +1><p style='line-height:1em'>{enroll}</p></font>"))
  })
  
  output$ihe_percentile_info <- renderText({
    if(is.null(input$dname)) return()
    
    paste("within", sector_choice())
    
    #HTML(glue("within <b>{sector_info}</b><br>
    #      instituions of higher education"))
  })
  
  output$institution_graduation <- renderText({
    if(is.null(input$dname)) return()
    
    print({dname_choice() %>%  distinct(dname) %>% pull(dname)})
  })
  
  output$ihe_sector_dist_info <- renderText({
    if(is.null(input$dname)) return()
    
    print(sector_choice())
    
    #HTML(glue("within <b>{sector_info}</b><br>
    #      instituions of higher education"))
  })
  
  
  output$ihe_enrollment <- renderText({
    if(is.null(input$dname)) return()
    
    profile_react() %>%
      distinct(dname,year,enroll) %>%
      #select(dname, year, enroll) %>% 
      #ungroup() %>% 
      filter(year== max(year)) %>%
      pull(enroll)
    
    #HTML(glue("<b>Fulltime Enrollment (undergrad | grad)</b>
    #          <font size = +1><p style='line-height:1em'>{enroll}</p></font>"))
  })
  
  
  
  output$ihe_graduation <- renderText({
    if(is.null(input$dname)) return()
    
    profile_react() %>%
      distinct(dname,year,completion_rate_per_inst_year) %>%
      #filter(year == 2019) %>%
      filter(year==max(year)) %>%
      mutate(completion_rate_per_inst_year=scales::percent(completion_rate_per_inst_year)) %>%
      pull(completion_rate_per_inst_year) 
    
    # HTML(glue("<font size = +1><p style='line-height:0.25em'>{grand_completion_rate}</p></font>"))
  })
  
  output$ihe_tuition <- renderText ({
    if(is.null(input$dname)) return()
    
    profile_react() %>% 
      distinct(dname,year,tuition_under) %>%
      #filter(year == 2019) %>%
      filter(year==max(year)) %>%
      pull(tuition_under)
    
    #HTML(glue("<b><font size = 0><p style='line-height:0.25em'>Tuition<br>(in state | out of state)</b></font></p>
    #          <font size = +1><p style='line-height:0.25em'>{tuition}</b></font></p>"))
    
  })
  
  output$ihe_pell <- renderText ({
    if(is.null(input$dname)) return()
    
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname == input$dname) %>% 
      filter(year == max(year)) %>% 
      drop_na(pell_pct_per_inst_year) %>% 
      mutate(pell_pct_per_inst_year = paste0(pell_pct_per_inst_year*100, "%")) %>%
      pull(pell_pct_per_inst_year) %>% 
      unique()
  })
  
  # -----. Rawvalue generic reactive
  
  
  output$ihe_retention <- renderText ({
    if(is.null(input$dname)) return()
    
    # retention <- efa_fsums_pell_compl_profile_equity %>%
    #   filter(dname == input$dname) %>%
    #   select(year, ft_retention) %>% 
    #   filter(year == max(year)) %>% 
    #   mutate(ft_retention_pct = paste0(round(ft_retention, 0), "%")) %>%
    #   distinct() %>% 
    #   pull(ft_retention_pct)
    
    retention <- raw_values %>%
      filter(dname==input$dname &
               variable =="retention_rate" &
               #year == "2019") %>%
               year==max(year)) %>%
      select(dname, year,variable, value) %>%
      rename(retention='value') %>%
      mutate(retention=scales::percent(retention)) %>%
      distinct() %>% 
      pull(retention)
    
    # HTML(glue("<b><font size = 0><p style='line-height:0.25em'>Retention Rate<br></b></font></p>
    #          <font size = +1><p style='line-height:0.25em'>{retention}</b></font></p>"))
    
  })
  
  output$ihe_retention_year <- renderUI({
    if(is.null(input$dname)) return()
    
    span(#HTML("<b><font size = 0><p style='line-height:0.25em'>Retention Rate</b></font></p>"),
      tipify(el = icon(name = "question-circle", class="profile_icon", lib = "font-awesome"), 
            # title = "Year: 2020, Full-time retention rate is the percent of the fall full-time cohort from the prior year that re-enrolled at the institution in the current year.",
             title = raw_values %>%
               filter(dname==input$dname &
                        variable =="retention_rate" &
                        year==max(year)) %>%
               mutate(year=paste0("Year: ", .$year, "; Full-time retention rate is the percent of the fall full-time cohort from the prior year that re-enrolled at the institution in the current year.")) %>%
              pull(year),
             placement=NULL, 
             trigger = "hover",
            options = list(width = "200px")))
    
  })
  
  
  output$ihe_pell_pct <- renderUI ({
    if(is.null(input$dname)) return()
    
    pell_pct <- profile_efa_fsums_total %>% 
      select(dname, pell_pct_per_inst) %>%
      filter(dname==input$dname) %>%
      mutate(pell_pct_per_inst=scales::percent(pell_pct_per_inst)) %>%
      pull(pell_pct_per_inst)
    
    HTML(glue("<b><font size = 0><p style='line-height:0.25em'>Pell (%)<br></b></font></p>
              <font size = +1><p style='line-height:0.25em'>{pell_pct}</b></font></p>"))
    
  })
  
  output$ihe_endowment <- renderText ({
    if(is.null(input$dname)) return()
    
    raw_values %>% 
      filter(dname==input$dname &
               str_detect(variable, "endowment") &
               #year == "2019") %>% 
               year==max(year)) %>%
      mutate(value=scales::dollar(value)) %>%
      select(dname,unitid,year,variable,value) %>% 
      distinct() %>%
      pull(value)
    
    #HTML(glue("<b><font size = 0><p style='line-height:0.25em'>Endowment (USD)<br></b></font></p>
    #          <font size = +1><p style='line-height:0.25em'>{endowment}</b></font></p>"))
    
  })
  
  output$ihe_endowment_year <- renderUI({
    if(is.null(input$dname)) return()
    
    span(#HTML("<b><font size = 0><p style='line-height:0.25em'>Retention Rate</b></font></p>"),
      tipify(el = icon(name = "question-circle", class="profile_icon", lib = "font-awesome"), 
             title = raw_values %>%
               filter(dname==input$dname & 
                        str_detect(variable, "endowment") & 
                        #year == "2019") %>% 
                        year==max(year)) %>%
               mutate(year=paste0("Year: ", .$year, "; Value of endowment assets at the end of the fiscal year")) %>%
               pull(year), 
             placement=NULL, 
             trigger = "hover"))
    
  })
  
  instruction_cost_cal<-reactive({
    
    raw_values %>% 
      #filter(dname=="Alabama A & M University (100654)") %>%
      #filter(dname == input$dname) %>% 
      select(dname, year,variable, value) %>%
      # instruction cost per student = instruction_current_year_total/enrollment in the same year
      filter(variable=="instruction_current_year_total" | variable=="enrollment" | variable == "instruction_total_amount") %>%
      filter(year==max(year)) %>% 
      #group_by(dname) %>%
      #filter(year == 2021) %>%
      
      group_by(dname) %>% 
      distinct() %>% 
      #mutate(n=n()) #%>% 
      #filter(n==2) %>%
      summarize(instruction_cost_per_stu=lag(value)/value) %>%
      drop_na(instruction_cost_per_stu) %>%
      mutate(instruction_cost_per_stu=scales::dollar(instruction_cost_per_stu)) %>%
      right_join(fsums, by=c("dname")) %>%
      select(dname, instruction_cost_per_stu) %>%
      distinct()
    
  })
  
  output$ihe_instruction_cost <- renderText ({
    if(is.null(input$dname)) return()
    
    instruction_cost_cal() %>% 
      filter(dname==input$dname) %>% 
      pull(instruction_cost_per_stu)
    
    # HTML(glue("<b><font size = 0><p style='line-height:0.25em'>Instruction Cost (per Student USD)<br></b></font></p>
    #            <font size = +1><p style='line-height:0.25em'>{instruction_cost}</b></font></p>"))
    
  })
  
  output$ihe_instruction_cost_description <- renderUI({
    if(is.null(input$dname)) return()
    
    # The whole data points are from 2019
    span(#HTML("<b><font size = 0><p style='line-height:0.25em'>Retention Rate</b></font></p>"),
      tipify(el = icon(name = "question-circle", class="profile_icon", lib = "font-awesome"), 
             title =
               "Year: 2021; The sum of all operating expenses associated with the colleges, schools, departments, and other instructional divisions.",
             #ipeds_dictionary %>%
             #filter(var_column=="instruction_current_year_total") %>%
             #pull(description),
             placement=NULL, 
             trigger = "hover",
             options=list(width="200px")))
    
  })
  
  
  
  # output$ihe_selectivity <- renderText ({
  #   if(is.null(input$dname)) return()
  #   
  #   efa_fsums_pell_compl_profile_equity %>% 
  #     filter(dname==input$dname) %>% 
  #     mutate(pct_admitted_pct = paste0(round(pct_admitted, 0), "%")) %>%
  #     pull(pct_admitted_pct) %>% unique()
  #   
  # })
  
  selectivity_pct <- reactive ({
    if(is.null(input$dname)) return()
    
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname==input$dname) %>% 
      drop_na(pct_admitted) %>% 
      mutate(pct_admitted_pct = paste0(round(pct_admitted, 0), "%")) %>%
      filter(year == max(year)) %>% 
      pull(pct_admitted_pct) %>% unique()
  })
  
  selectivity <- reactive ({
    efa_fsums_pell_compl_profile_equity %>% 
      filter(dname==input$dname) %>% 
      filter(year == max(year)) %>% 
      drop_na(pct_admitted_threshold) %>% 
      pull(pct_admitted_threshold) %>% unique()
    
  })
  
  output$ihe_selectivity <- renderText ({
    paste(selectivity_pct(), "|", selectivity())
  })
  
  
  #raw values of endowment by institution
  
  
  ## Financial Tab ----
  #{
  # output$select_sector <- renderUI({
  #   
  #   sector_list <- fsums %>% 
  #     #filter(state == input$state) %>% 
  #     distinct(sector) %>% 
  #     pull(sector)
  #   
  #   selectInput(
  #     "sector", 
  #     label = "Sector", 
  #     choices = sort(sector_list),
  #     selected = "Public, 4-year or above"
  #     )
  # })
  
  # output$select_inst <- renderUI({
  #   
  #   names_list <- fsums %>% 
  #     filter(state == input$state) %>% 
  #     filter(sector == input$sector) %>% 
  #     distinct(dname) %>% 
  #     pull(dname)
  #   
  #   selectizeInput(
  #     inputId = "dname",
  #     label = "Select University",
  #     choices = sort(names_list),
  #     options = list(create = TRUE,
  #                    maxOptions = 5,
  #                    placeholder = 'Select up to 5')#,
  #    # selected = "University of Utah (230764)"
  #   )
  # })
  # }
  
  
  
  ### Main distribution plot -------------------------------------------------
  
  # React avg.s --------------------
  
  # State_avg. -----
  
  dname_choice<-reactive({
    
    fsums %>%
      ungroup() %>%
      filter(dname==input$dname)
    
  })
  
  
  state_choice<-reactive({    
    
    fsums %>%
      filter(dname==input$dname) %>%
      distinct(state) %>% 
      pull(state) %>%
      unique
    
  })
  
  state_avg_react<-reactive({
    
    if(!isTRUE(input$state_avg)){return()}
    fsums %>% 
      ungroup() %>%
      filter(state==state_choice() & sector==sector_choice()) %>%
      group_by(state) %>%
      summarise(total=mean(total, na.rm=T)) %>%
      mutate(total=round(total, 3)) %>%
      pull(total) 
    #mutate(y=8) %>%
    #rename(dname= "state")
    
  })
  
  
  # Sector avg. -----
  
  sector_avg_react<-reactive({
    
    if(!isTRUE(input$sector_avg)){return()}
    fsums %>% 
      ungroup() %>%
      filter(sector==sector_choice()) %>%
      group_by(sector) %>%
      summarise(total=mean(total, na.rm=T)) %>%
      mutate(total=round(total, 3)) %>%
      pull(total) 
    #mutate(y=8) %>%
    #rename(dname= "state")
    
  })
  
  # Top performers in your state. -----
  
  top_state_react<-reactive({
    
    if(!isTRUE(input$top_state)){return()}
    
    fsums %>%
      filter(state==state_choice() & sector==sector_choice()) %>%
      distinct(dname, total) %>%
      ungroup() %>%
      slice_max(total,n = 3) %>%
      arrange(dname) %>%
      slice_head(n = 3) %>%
      mutate(y=4)
    
  })
  
  # Top performers in your state. -----
  
  top_sector_react<-reactive({
    
    if(!isTRUE(input$top_sector)){return()}
    
    fsums %>%
      filter(sector==sector_choice()) %>%
      distinct(dname, total) %>%
      ungroup() %>%
      slice_max(total,n = 3) %>%
      arrange(dname) %>%
      slice_head(n = 3) %>%
      mutate(y=4)
    
  })
  
  ds_react<-reactive({
    
    ds<-density(fsums %>%
                  distinct(sector,total,dname,pct_rank,pct_rank_raw,unitid) %>%
                  filter(sector %in% c(sector_choice())) %>%
                  #filter(sector %in% c("Private, 4-year or above")) %>%
                  #mutate(pct_scr=str_remove_all(pct_rank, "[[:alpha:]]")) %>%
                  #mutate(pct_scr=as.numeric(pct_scr)) %>%
                  #pull(pct_scr))
                  pull(total))
    data.frame(x=ds$x, y=ds$y)
    
  })
  
  coordinate_react<-reactive({
    
    total<-fsums %>% 
      distinct(sector, total, dname, pct_rank, pct_rank_raw,unitid) %>%
      filter(dname %in% c(input$dname)) %>%
      #mutate(pct_scr=str_remove_all(pct_rank, "[[:alpha:]]")) %>%
      #mutate(pct_scr=as.numeric(pct_scr)) %>%
      #pull(pct_scr)
      pull(total)
    
    data.frame(x=total,y=6)
    #    sapply(total, function(i) {ds_react() %>%
    #        filter(x>i & x<(i+0.02)) %>% 
    #        slice(which.max(x)) %>%
    #        round(.,5)}) %>%
    #      t(.) %>%
    #      as.data.frame %>%
    #      rownames_to_column() %>%
    #      mutate(coloract = ifelse(rowname=='1', "orange", "#7e95ac")) %>%
    #      mutate(cluster = ifelse(rowname=='1', "prime", "others"))
    
  })
  
  coordinate_react_comp<-reactive({
    
    fsums %>% 
      distinct(sector, total, dname, pct_rank) %>%
      #mutate(pct_scr=str_remove_all(pct_rank, "[[:alpha:]]")) %>%
      #mutate(pct_scr=as.numeric(pct_scr)) %>%
      filter(dname %in% c(input$dname,
                          input$dname_comp)) %>%
      mutate(cluster = ifelse(dname==input$dname, "prime", "others")) %>%
      arrange(desc(cluster)) %>%
      select(dname, total) %>%
      mutate(y=6) %>%
      bind_rows(.,top_state_react()) %>%
      bind_rows(.,top_sector_react())
    
  })
  


  

  
  
  ### Subscore plot -----------------------------------------------------------
  
  #. ----- Drilldown reactives
  
  drilldown_react_df<-reactive({
    fsums %>% 
      mutate(category_definition = category.x) %>% 
      mutate_at(c("category_definition"), str_replace_all, pattern = "Assets", replacement = "Assets - A resource of economic value") %>% 
      mutate_at(c("category_definition"), str_replace_all, pattern = "Debt", replacement = "Debt - An amount owed for funds borrowed") %>% 
      mutate_at(c("category_definition"), str_replace_all, pattern = "Expenses", replacement = "Expenses - The reduction in value of an asset as it is used to generate revenue") %>% 
      mutate_at(c("category_definition"), str_replace_all, pattern = "Revenue", replacement = "Revenue - An increase in assets or decrease in liabilities caused by the provision of services or products to customers") %>% 
      mutate_at(c("category_definition"), str_replace_all, pattern = "Student", replacement = "Student") %>% 
      ungroup() %>% 
      filter(dname==input$dname) %>%
      select(dname, category.x,score_per_inst_category,nice_name,weighted_value, category_definition) %>%
      rename("name"=nice_name,"value"=weighted_value, "category"=category.x,) %>%
      arrange(name) %>% 
      nest(-c(dname,category,score_per_inst_category,category_definition)) %>%
      arrange(factor(category, levels = c("Revenue", "Expenses", "Assets", "Debt", "Students")))
    
  })
  
  drilldown_react_plot<-reactive({
    
    if(is.null(input$dname)) return()
    
    #    canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.category]);}")
    #    legendClickFunction <- JS("function(event) {Shiny.onInputChange('legendClicked', this.name);}")
    
    title_with_score <- dname_choice() %>% 
      distinct(institution_entity_name, total, pct_rank) %>% 
      mutate(title = glue("{institution_entity_name} Index Score: {round(total, 2)} ({pct_rank} Percentile)")) %>% 
      pull(title)
    
    # Highchart - drilldown per category
    
    highchart() %>%
      hc_xAxis(type = "category") %>%
      hc_add_series(
        data = drilldown_react_df() %>%
          select(dname,category,score_per_inst_category) %>%
          mutate(drilldown=tolower(category)) %>%
          rename("name"=category, "y"=score_per_inst_category),
        type = "lollipop",
        hcaes(name = name, y = y),
        name = "Category",
        colorByPoint = TRUE,
        connectorWidth=5,
        marker=list(radius=5)
      ) %>%
      hc_yAxis(min=-2, max = 2, 
               tickInterval=0.5,
               labels = list(style = list(fontSize = "2vh")),
               title = list(text = "FHI Sub-Items",
                            style = list(fontSize = "3vh",
                                         fontWeight="bold"))) %>%
      hc_xAxis(
        labels = list(style = list(fontSize = "2vh"),
                      enabled = TRUE,
                      useHTML = TRUE
        ),
        title = list(text = "FHI Score",
                     style = list(fontSize = "3vh",
                                  fontWeight="bold"))) %>%
      #hc_add_dependency("plugins/grouped-categories.js") %>%
      hc_chart(inverted = T) %>%
      hc_size(width = 500, height = 400) %>%
      hc_legend(enable=FALSE) %>%
      hc_plotOptions(
        series = list(
          boderWidth = 10,
          dataLabels = list(enabled = TRUE),
          events = list(
            mouseOver = JS("function() { if(this.options.color !== 'red') {this.update({color: 'red'})} }"),
            mouseOut = JS("function() { if(this.options.color === 'red') {this.update({color: '#ddd'})} }")
          )
        )
      )
    
  })
  
  # Drilldown1. Sub-items
  
  output$subscorePlot <-  renderHighchart({
    
    drilldown_react_plot() %>%
      hc_drilldown(
        allowPointDrilldown = TRUE,
        series = list(
          
          list(
            type = "lollipop",
            id = "assets",
            data = drilldown_react_df() %>%
              filter(category=="Assets") %>%
              select(data) %>%
              unnest(data) %>%
              #arrange(name) %>%
              list_parse2()
          ),
          list(
            type = "lollipop",
            id = "debt",
            data = drilldown_react_df() %>%
              filter(category=="Debt") %>%
              select(data) %>%
              unnest(data) %>%
              #arrange(name) %>%
              list_parse2()
          ),
          list(
            type = "lollipop",
            id = "expenses",
            data =  drilldown_react_df() %>%
              filter(category=="Expenses") %>%
              select(data) %>%
              unnest(data) %>%
              #arrange(name) %>%
              list_parse2()
          ),
          list(
            type = "lollipop",
            id = "revenue",
            data = drilldown_react_df() %>%
              filter(category=="Revenue") %>%
              select(data) %>%
              unnest(data) %>%
              #arrange(name) %>%
              list_parse2()
          ),
          list(
            type = "lollipop",
            id = "student",
            data = drilldown_react_df() %>%
              filter(category=="Student") %>%
              select(data) %>%
              unnest(data) %>%
              #arrange(name) %>%
              list_parse2()
          )
        )
      )
    #    %>%
    #      hc_plotOptions(series = list(stacking = FALSE, events = list(click = canvasClickFunction, legendItemClick = legendClickFunction)))
    
  })
 
  
  makeReactiveBinding("outputTable")
  makeReactiveBinding("outputText")
  makeReactiveBinding("outputPlot")
  
  current_feature <- eventReactive(input$canvasClicked[2], {
    
    cf<-features %>%
      filter(nice_name == input$canvasClicked[2]) %>% 
      filter(control_of_institution == (fsums %>% 
                                          filter(dname == input$dname) %>% 
                                          distinct(control_of_institution) %>% 
                                          pull(control_of_institution)))
    return(cf)
  })
  
  variables <- eventReactive(input$canvasClicked[2], {
    
    req(current_feature())
    
    variables <- current_feature() %>% 
      distinct(variable) %>% 
      pull(variable)
    
    return(variables)
  })
  
  raw_features_data <- eventReactive(input$canvasClicked[2], {
    
    req(current_feature())
    req(variables())
    
    rfd <- raw_values %>% 
      filter(dname == input$dname) %>% 
      filter(variable %in% variables()) %>% 
      left_join(current_feature() %>% select(variable, fraction_part, sequence, var_title, description)) %>% 
      arrange(year, desc(fraction_part), sequence) %>% 
      deselect(fraction_part, sequence) %>%
      distinct()
    
    return(rfd)
  })
  
  
  charts <- eventReactive(input$canvasClicked[2], {
    
    req(current_feature())
    req(variables())
    req(raw_features_data())
    
    #outputText <<- paste0("You clicked on series ", input$canvasClicked[1], " and the bar you clicked was from category ", input$canvasClicked[2], ".") 
    charts <- lapply(1:length(variables()), function(x) {
      raw_features_data() %>%
        filter(variable == variables()[x]) %>%
        hchart("line",
               hcaes(x=year, y=value), 
               animation=T, 
               allowPointSelect=T,
               marker=list(radius=3),
               margin= list(5, 5, 5, 5)) %>%
        hc_title(
          text = raw_features_data() %>%
            filter(variable == variables()[x]) %>% pull(var_title) %>% unique(),
          margin = 20,
          align = "left",
          style = list(fontSize = "2vh",
                       fontWeight = "bold",
                       color = "black")
        ) %>%
        hc_subtitle(text = raw_features_data() %>%
                      filter(variable == variables()[x]) %>% pull(description) %>% unique(),
                    align = "left") %>%
        hc_size(width = 500) %>%
        hc_xAxis(labels = list(rotation=-45))
    })
    
    charts2<-hw_grid(charts, ncol=2,rowheight = 400)
    
    return(charts2)
  })
  
  text_formula <- eventReactive(input$canvasClicked[2], {
    
    req(current_feature())
    
    nice_var_features <- current_feature() %>% 
      select(-dname) %>% 
      distinct() %>% 
      mutate(numerator_operation=case_when(numerator_operation=="plus"~"+",
                                           numerator_operation=="minus"~"-")) %>%
      # Replace hyphen - with other marks
      mutate(var_title=str_replace_all(var_title, "-","")) %>%
      mutate(var_title=str_replace_all(var_title, "( )|(\\, )", "\\\\,")) %>%
      mutate(formula=str_c(var_title,numerator_operation, sep=" ")) %>%
      # Function to breaklines when there are more than 5 sub-variables
      # mutate(formula=if_else(sequence==5, paste(formula,"\n"), formula)) %>%
      select(-sequence) %>%
      distinct() %>%
      group_by(fraction_part) %>%
      mutate(id = row_number()) %>%
      mutate(formula=case_when(id%%3==0~paste(formula, callapse="\\\\"),
                               TRUE~formula)) %>%
      mutate(formula=paste(formula,collapse = "\\,")) %>%
      mutate(formula=str_replace(formula,"(\\+|\\-|\\+ \\\\\\\\)$","")) %>%
      mutate(formula=case_when(formula!="NA"~formula)) %>%
      mutate(formula=case_when(is.na(formula)~as.character(var_title),
                               TRUE~formula)) %>%
      distinct(derived, fraction_part, formula) 
    
    return(nice_var_features)
    
  })
  
  
  observeEvent(input$canvasClicked[2], {
    
    outputPlot<<-charts()
    
  })
  observeEvent(input$canvasClicked[2], {
    
    outputText <<- paste0("You clicked on series ", input$canvasClicked[1], " and the bar you clicked was from category ", input$canvasClicked[2], ".") 
    
  })
  
  
  output$SubRawplot <- renderUI({ 
    
    outputPlot
    
  })
  
  output$table<- renderTable({
    outputTable      
  })
  
  output$text<- renderText({
    outputText     
  })
  
  # https://math.meta.stackexchange.com/questions/5020/mathjax-basic-tutorial-and-quick-reference
  output$formula <- renderUI({
    withMathJax(helpText(paste(
      input$canvasClicked[2],
      "$$\\frac{", text_formula() %>% filter(fraction_part=="numerator") %>% pull(formula),"}",
      '{',text_formula() %>% filter(fraction_part=="denominator") %>% pull(formula),'}\\!$$')))
  })
  
  #  withMathJax(helpText(paste(
  #    #input$canvasClicked[2],
  #    "Govt Grants Contracts as % Total Revenue",
  #    "$$\\frac{", nice_var_features %>% filter(fraction_part=="numerator") %>% pull(formula),"}",
  #    '{',nice_var_features %>% filter(fraction_part=="denominator") %>% pull(formula),'}\\!$$')))
  
  

  
  # Big Picture Analysis Tab -------------------------------------------------------------
  
  # Other features should be included
  # from efa_fsums_pell_compl_profile_equity -> 1. size(enrollment), 2. pell_pct, 3. grand_completion_rate, 4. tuition_under, 5.location, 6. proportion of minority students
  # from hd -> 1. status_of_instution, 2. state, 3. size+region
  # from efa -> 1. proportion of full time student
  
  # Equity Dash tab -------------------------------------------------------------
  
  enroll_grad_column <- reactive({
    efa_fsums_pell_compl_profile_equity %>%
      select(dname, race, enrollment_pct_per_inst_year_race, year, completers_pct_per_inst_year_race) %>%
      filter(dname== input$dname) %>%
      group_by(race) %>%
      mutate(mean_enrollment = mean(enrollment_pct_per_inst_year_race)*100) %>%
      mutate(mean_graduation = mean(completers_pct_per_inst_year_race)*100) %>%
      distinct(dname, race, mean_enrollment, mean_graduation) %>% 
      arrange(desc(mean_graduation), desc(mean_enrollment))
    
  })
  
  
  output$enroll_grad_column_chart <- renderHighchart({
    
    validate(
      need(input$dname != "--none--", 'Please Choose Institution')
    )
    
    
    highchart() %>%
      hc_chart(type = "column", inverted = TRUE) %>%
      hc_xAxis(categories = enroll_grad_column()$'race') %>%
      hc_add_series(
        name = "Enrollment", 
        data = enroll_grad_column()$'mean_enrollment',
        style = list(color = "#3e6d72")
      ) %>% 
      hc_add_series(
        name = "Graduation", 
        data = enroll_grad_column()$'mean_graduation',
        style = list(color = "#000000")
      ) %>% 
      hc_title(text = "Enrollment compared with Graduation Rates, by Race") %>% 
      hc_size(width = 800)
    
  })
  
  parallel_chart<-reactive({
    
    efa_fsums_pell_compl_profile_equity %>%
      filter(dname=="University of Utah (230764)") %>%
      group_by(dname) %>%
      filter(year == 2019) %>%
      #filter(year==max(year)) %>%
      #filter(race %in% input$racechoice) %>%
      select(dname,year,race,
             pop_count_per_natl_year_race,
             pop_pct_per_natl_year_race,
             pop_count_per_state_year_race,
             pop_pct_per_state_year_race,
             enrollment_count_per_inst_year_race,
             enrollment_pct_per_inst_year_race,
             instructor_count_per_inst_year_race,
             instructor_pct_per_inst_year_race,
             completers_count_per_inst_year_race,
             completers_pct_per_inst_year_race) %>%
      pivot_longer(c(4:13),names_to="var_names",values_to="pct_count_values") %>%
      filter(str_detect(var_names,"_pct_"))
    
    
  })
  
  output$parallelOutput <- renderHighchart({
    
    validate(
      need(input$dname != "--none--", 'Please Choose Institution')
    )
    
    parallel_chart() %>%
      hchart("spline",
             hcaes(x = var_names, y = pct_count_values, group=race), 
             animation=T, 
             allowPointSelect=T,
             connectorWidth=0.3,
             marker=list(radius=5)
      ) %>%
      #              AIAN       ASIAN   Black      Hispanic  NHPI      Race Unk  2 or more white
      hc_colors(c( "#9e9947", "#0685ed","#24aa90","#ed1606","#fed976","#7e95ac","#800026","#e9b73f")) %>%
      hc_yAxis(
        gridLineDashStyle= 'longdash',
        gridLineWidth= 0.1,
        plotBands = list(
          list(
            from = parallel_chart() %>% 
              filter(race == "AIAN" & var_names=="pop_pct_per_state_year_race") %>%
              pull(pct_count_values),
            to = parallel_chart() %>% 
              filter(race == "AIAN" & var_names=="completers_pct_per_inst_year_race") %>%
              pull(pct_count_values),
            color = hex_to_rgba("#9e9947", 0.1)
          ),
          list(
            from = parallel_chart() %>% 
              filter(race == "Asian" & var_names=="pop_pct_per_state_year_race") %>%
              pull(pct_count_values),
            to = parallel_chart() %>% 
              filter(race == "Asian" & var_names=="completers_pct_per_inst_year_race") %>%
              pull(pct_count_values),
            color = hex_to_rgba("#24aa90", 0.1)
          ),
          list(
            from = parallel_chart() %>% 
              filter(race == "Hispanic" & var_names=="pop_pct_per_state_year_race") %>%
              pull(pct_count_values),
            to = parallel_chart() %>% 
              filter(race == "Hispanic" & var_names=="completers_pct_per_inst_year_race") %>%
              pull(pct_count_values),
            color = hex_to_rgba("#ed1606", 0.1)
          ),
          list(
            from = parallel_chart() %>% 
              filter(race == "Black" & var_names=="pop_pct_per_state_year_race") %>%
              pull(pct_count_values),
            to = parallel_chart() %>% 
              filter(race == "Black" & var_names=="completers_pct_per_inst_year_race") %>%
              pull(pct_count_values),
            color = hex_to_rgba("#0685ed", 0.1)
          ),
          list(
            from = parallel_chart() %>% 
              filter(race == "NHPI" & var_names=="pop_pct_per_state_year_race") %>%
              pull(pct_count_values),
            to = parallel_chart() %>% 
              filter(race == "NHPI" & var_names=="completers_pct_per_inst_year_race") %>%
              pull(pct_count_values),
            color = hex_to_rgba("#fed976", 0.1)
          ),
          list(
            from = parallel_chart() %>% 
              filter(race == "Race Unknown" & var_names=="pop_pct_per_state_year_race") %>%
              pull(pct_count_values),
            to = parallel_chart() %>% 
              filter(race == "Race Unknown" & var_names=="completers_pct_per_inst_year_race") %>%
              pull(pct_count_values),
            color = hex_to_rgba("#7e95ac", 0.1)
          ),
          list(
            from = parallel_chart() %>% 
              filter(race == "Two or More Races" & var_names=="pop_pct_per_state_year_race") %>%
              pull(pct_count_values),
            to = parallel_chart() %>% 
              filter(race == "Two or More Races" & var_names=="completers_pct_per_inst_year_race") %>%
              pull(pct_count_values),
            color = hex_to_rgba("#800026", 0.1)
          ),
          list(
            from = parallel_chart() %>% 
              filter(race == "White" & var_names=="pop_pct_per_state_year_race") %>%
              pull(pct_count_values),
            to = parallel_chart() %>% 
              filter(race == "White" & var_names=="completers_pct_per_inst_year_race") %>%
              pull(pct_count_values),
            color = hex_to_rgba("#e9b73f", 0.1)
          )
        )
      ) %>%
      hc_xAxis(
        plotBands = list(
          list(
            from = -0.5,
            to = 1.5,
            color = hex_to_rgba("red", 0.01),
            label = list(text = "Cohort")
          )
        ),
        plotLines = list(
          list(
            label = FALSE,
            color = "black",
            width = 0.5,
            value = 0
          ),
          list(
            label = FALSE,
            color = "black",
            width = 0.5,
            value = 1
          ),
          list(
            label = FALSE,
            color = "black",
            width = 0.5,
            value = 2
          ),
          list(
            label = FALSE,
            color = "black",
            width = 0.5,
            value = 3
          ),
          list(
            label = FALSE,
            color = "black",
            width = 0.5,
            value = 4
          )
        )
      )
  })
  
  
  
  
  
  
  #. React Table
  
  output$equityTable<-renderReactable({
    
    validate(
      need(input$dname != "--none--", 'Please Choose Institution')
    )
    
    data1<-efa_fsums_pell_compl_profile_equity %>%
      filter(dname=="University of Utah (230764)") %>%
      group_by(dname) %>%
      #filter(year==max(year)) %>%
      ungroup() %>%
      #filter(race %in% input$racechoice) %>%
      select(race,year,
             pop_count_per_natl_year_race,
             pop_pct_per_natl_year_race,
             pop_count_per_state_year_race,
             pop_pct_per_state_year_race,
             enrollment_count_per_inst_year_race,
             enrollment_pct_per_inst_year_race,
             instructor_count_per_inst_year_race,
             instructor_pct_per_inst_year_race,
             completers_count_per_inst_year_race,
             completers_pct_per_inst_year_race) %>%
      pivot_longer(c(3:12), names_to="var_names", values_to="values") %>%
      mutate(count_type=case_when(str_detect(var_names,"_count_")~"count",
                                  str_detect(var_names,"_pct_")~"pct")) %>%
      mutate(var_names=str_remove_all(var_names, "(_count|_pct)")) %>%
      pivot_wider(names_from="count_type", values_from="values") %>%
      select(var_names, year, race, count, pct) %>%
      group_by(var_names, race) %>%
      group_by(var_names,race) %>%
      mutate(mean_count=mean(count, na.rm=T), mean_pct=mean(pct, na.rm=T)) %>%
      mutate(pct=round(pct*100,3)) %>%
      mutate(timetrend=list(pct)) %>%
      select(-count,-pct,-year) %>%
      distinct %>%
      mutate(sparkline=NA) %>%
      mutate(var_names=case_when(var_names=='completers_per_inst_year_race'~"Completers",
                                 var_names=='enrollment_per_inst_year_race'~"Enrollment",
                                 var_names=='instructor_per_inst_year_race'~"Instructors",
                                 var_names=='pop_per_natl_year_race'~"Nation",
                                 var_names=='pop_per_state_year_race'~"State")) 
    #      data<-SharedData$new(data1)
    
    #  bscols(
    #    widths = c(2, 10),
    #    list(
    #      filter_checkbox("var_names", "Indicators", data, ~var_names),
    #      filter_checkbox("race", "Race", data, ~race)
    #    ),
    reactable(
      data1,
      pagination = FALSE,
      defaultSorted = "var_names",
      defaultColDef = colDef(headerClass = "header", align = "left"),
      columns = list(
        var_names = colDef(name = "Equity Indicators",
                           maxWidth = 150, 
                           align="center",
                           style = list(color="black"),
                           headerClass="profile_icon"),
        race = colDef(name = "Race",
                      maxWidth = 150, 
                      align="center",
                      style = list(color="black")),
        mean_count = colDef(name = "Count Avg.", 
                            maxWidth = 100, 
                            style = list(color="black"),
                            format = colFormat(separators = TRUE, digits = 0)),
        mean_pct = colDef(name = "Percent Avg.", 
                          maxWidth = 300,
                          format = colFormat(percent = TRUE, digits = 1),
                          cell = function(value) {
                            width=100
                            value=paste0(round(value*100,2),"%")
                            value <- format(value, width = 9, justify = "right")
                            bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
                          },
                          align = "center",
                          style = list(fontFamily = "monospace", whiteSpace = "pre", color="black")
        ),
        timetrend = colDef(show = F),
        sparkline = colDef(name = "Time Trend", 
                           align="center",
                           maxWidth = 300, 
                           style = list(color="black"),
                           cell = function(value, index) {
                             sparkline::sparkline(data1$timetrend[[index]],width = 100)
                           }
        )
      ),
      height = 500,
      searchable = TRUE,
      filterable = TRUE
    )
    
    #  )
  })
  
  # Back to top ---------   
  
  observeEvent(input$toTop, {
    js$toTop();
  })
  
  
}