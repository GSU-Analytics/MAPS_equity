

#source("global.R") 

# body ----

body <-   mainPanel(width=12,
                    tags$style(type="text/css",
                               ".shiny-output-error { visibility: hidden; }",
                               ".shiny-output-error:before { visibility: hidden; }"
                    ),
                    style="margin-bottom: 100px",
                    align= "center",
                    
                    tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
                    absolutePanel(
                      top = 360, left = 450, right = 0, 
                      div(
                        img(src = "./images/teens for FHII.png", height = 420,
                            style = "z-index: 1000000000;")
                      ),
                      style = "z-index: 100000 !important;"
                    ),
                    fluidRow(
                      column(align = "left",
                             6,
                             tags$a(
                               img(src = "./images/MAPS logo.png", class="logoImg",
                                   style = "margin-left: 50px !important;")
                             )),
                      column(6,
                             align = "right",
                             tags$a(
                               img(src = "./images/logo.png",width="70%", height="70%",
                                   style = "margin: 70px 25px 50px 0px !important;")
                             ))
                    ),
                    fluidRow(
                      column(align="left",style = "margin: 50px 50px 10px 0px",
                             12,
                             div(class="main_title",
                                 "MAPS INSTITUTIONAL EQUITY OUTCOMES DASHBOARD")
                      )
                    ),
                    fluidRow(
                      column(
                        style="background-color:#000000; z-index: unset;",
                        12,
                        tags$br(),
                        
                        fluidRow(
                          column(
                            style="margin:10px 100px 50px 100px;",
                            7, align = "left",
                            div(class = "whitetxt",
                                "Expediting equitable outcomes for students is an important 
                                priority for higher education leaders. Boards, presidents, 
                                senior leadership teams, policymakers, and other stakeholders 
                                need tools to understand, measure, and improve institutional 
                                practices that will lead to more equitable outcomes for students. 
                                Measuring equity across the entire student journey - through 
                                access, experience, outcomes, and impact - is increasingly 
                                crucial as disparities between student groups along lines of 
                                race, gender, socioeconomic status, and more have accelerated 
                                in the wake of the COVID-19 pandemic, shifting national demographics, 
                                and disruptive industry trends. "
                            ),
                            br(),
                            div(class = "whitetxt",
                                "This dashboard uses IPEDS data from 2014-2021 to provide insight 
                                into disaggregated student outcomes and institutional practices 
                                that may generate greater equity. While institution-focused data are 
                                robust in IPEDS, few disaggregated student data points exist, 
                                leaving users with limited visibility into the state of equity across 
                                the nation with no industry-wide database alternatives. Our recent 
                                white paper Missing Metrics: An Analysis of Equity Measurement Gaps 
                                in Higher Education addresses these gaps and explores emerging solutions 
                                to promote system-wide equity measurement standards. Despite this 
                                challenge, this tool displays any currently available IPEDS data that 
                                may help measure equity indicators and student outcomes at colleges 
                                and universities. "
                          ),
                          br(),
                          div(class = "whitetxt",
                              "This work is a part of the MAPS Project, where we Model, Analyze, Prototype, 
                              and Share innovative data and solutions to challenges in higher education. ")
                          )
                        )
                      )
                    ),
                    br(),
                    
                    tags$br(),
                    # tags$br(),
                    
                    
                    
                    
                    fluidRow(
                      column(8,align="left",
                             style = "margin-left: 50px;",
                             div(class="lvl2_title",
                                 "SELECT YOUR SECTOR")
                      )
                    ),

                    fluidRow(
                      column(7, align="left",
                             style = "margin-left: 50px;",
                             
                             uiOutput("sector")
                             
                      ),
                      column(4,
                             uiOutput("state"),
                             
                             uiOutput("dname") #%>% withSpinner(color="#0dc5c1")
                      )
                    ),
                    br(),
                    br(),
                    fluidRow(align="left",
                             column(12,
                                    style='padding:10px; font-size: 5rem;
                           font-weight: 900; color:#3d6c72;
                           margin-left: 50px; margin-right: 100px',
                                    uiOutput("name_icon")
                                      )
                    ),
                    
                    # -. School Location
                    
                    fluidRow(align="left",
                             column(12,
                                    div(class="location",
                                        style = "font-size: 30px;",
                                        tags$head(tags$style("
                                                             #container * {
                                                             display: inline;
                                                             }")),
                                        uiOutput("location_icon"),
                                        uiOutput("hbcu"),
                                        uiOutput("tribal"),
                                        htmlOutput("land_grant")
                                        )
                             )
                    ),
                    
                    br(),
                    div(
                      # fluidRow(
                      #   style='background-color: #e9b73f; align: center; margin-left: 0px; position:sticky; top:5px; z-index:10',
                      #   column(12,
                      #          align = "left",
                      #          h5("School Profile"))
                      #   ),
                      
                      br(),
                      
                      br(),
                      
                      bsCollapse(id = "accordion", multiple = TRUE, 
                                 open = c("School Profile", "Services", "State and National Comparisons","Institutional Comparisons"),
                                 bsCollapsePanel("School Profile", style = "warning", id = "myaccordion",
                                                 # fluidRow(
                                                 #   column(12, align = "left",
                                                 #          div(
                                                 #            HTML("All data shown is from IPEDS 2019"),
                                                 #            style  = "color: black; font-size: 1em;
                                                 #            font-family: 'Roboto', sans-serif;
                                                 # 
                                                 #       margin-left: 55px;"))
                                                 # ),
                                                 fluidRow(
                                                   div(class="school_info",
                                                       
                                                       # -. Enrollment
                                                       
                                                       #column(1),
                                                       column(2, style="margin-right:50px; align: center;",
                                                              fluidRow( style = "align: center;",
                                                                        tipify(icon("question-circle", class="profile_icon",lib = "font-awesome"),
                                                                               "Year: 2021; Full-time enrollment students",
                                                                               placement=NULL,
                                                                               trigger = "hover")),
                                                              fluidRow(HTML("<font size = +2>Enrollment</font><br>
                                                                    undergrad | grad")),
                                                              fluidRow(style='border-top: 1px solid black; padding:10px;
                                                               font-size: 3rem;',
                                                                       textOutput("ihe_enrollment"))),
                                                       
                                                       # -. Graduation
                                                       
                                                       column(2,
                                                              style='margin-right:50px;',
                                                              fluidRow(
                                                                tipify(icon("question-circle", class="profile_icon",lib = "font-awesome"),
                                                                       "Year: 2021; Undergrad, the rate of full-time students that attain a bachelors degree within 150% of normal time",
                                                                       placement=NULL,
                                                                       trigger = "hover")),
                                                              fluidRow(
                                                                HTML("<font size = +2>Graduation (%)</font>")),
                                                              br(),
                                                              fluidRow(style='border-top: 1px solid black; padding:10px;
                                                           font-size: 3rem;',
                                                                       textOutput("ihe_graduation"))),
                                                       
                                                       # -. Tuition
                                                       column(3,
                                                              style='margin-right:50px;',
                                                              fluidRow(
                                                                tipify(icon("question-circle", class="profile_icon",lib = "font-awesome"),
                                                                       "Year: 2021; Undergrad, full-time students for the full academic year",
                                                                       placement=NULL,
                                                                       trigger = "hover")),
                                                              fluidRow(
                                                                HTML("<font size = +2>Tuition</font><br>
                                                            in-state | out-of-state")),
                                                              fluidRow(style='border-top: 1px solid black; padding:10px;
                                                           font-size: 3rem;',
                                                                       textOutput("ihe_tuition"))),
                                                       
                                                       # -. Pell Grant Recipients
                                                       column(3,
                                                              style = 'margin-right:50px;',
                                                              fluidRow(
                                                                tipify(icon("question-circle", class = "profile_icon", lib = "font-awesome"),
                                                                       "Year: 2021; Percent of undergraduate students awarded Pell grants",
                                                                       placement = NULL,
                                                                       trigger = "hover")
                                                              ),
                                                              fluidRow(
                                                                HTML("<font size = +2>Pell Grant Recipients</font><br>
                                                              undergrad")),
                                                              fluidRow(style = 'border-top: 1px solid black; padding:10px;
                                                            font-size: 3rem;',
                                                                       uiOutput("ihe_pell")
                                                              ))
                                                   )
                                                 ),
                                                 fluidRow(
                                                   div(class="school_info",
                                                       # -. Retention
                                                       column(2,
                                                              style="margin-right:50px;", #margin-left:120px",
                                                              fluidRow(uiOutput("ihe_retention_year")),
                                                              fluidRow(HTML("<font size = +2>Retention Rate</font>")),
                                                              br(),
                                                              fluidRow(style='border-top: 1px solid black; padding:10px;
                                                           font-size: 3rem;',
                                                                       textOutput("ihe_retention"))),
                                                       
                                                       # -. Endowment
                                                       column(2,
                                                              style='margin-right:50px;',
                                                              fluidRow(uiOutput("ihe_endowment_year")),
                                                              fluidRow(HTML("<font size = +2>Endowment</font>")),
                                                              br(),
                                                              fluidRow(style='border-top: 1px solid black; padding:10px;
                                                           font-size: 3rem;',
                                                                       textOutput("ihe_endowment"))),
                                                       
                                                       # -. Instruction Cost
                                                       column(3,
                                                              style='margin-right:50px;',
                                                              fluidRow(uiOutput("ihe_instruction_cost_description")),
                                                              fluidRow(HTML("<font size = +2>Instruction Cost</font><br>
                                                                per student")),
                                                              fluidRow(style='border-top: 1px solid black; padding:10px;
                                                           font-size: 3rem;',
                                                                       textOutput("ihe_instruction_cost"))),
                                                       
                                                       # -. Selectivity
                                                       column(3,
                                                              style='margin-right:50px;',
                                                              fluidRow(
                                                                tipify(icon("question-circle", class = "profile_icon", lib = "font-awesome"),
                                                                       "Year: 2021; Undergraduate, full time students",
                                                                       placement = NULL,
                                                                       trigger = "hover")
                                                              ),
                                                              fluidRow(HTML("<font size = +2>Selectivity</font><br>
                                                                percent admitted total")),
                                                              fluidRow(style='border-top: 1px solid black; padding:10px;
                                                           font-size: 3rem;',
                                                                       textOutput("ihe_selectivity")))
                                                   )
                                                 )
                                 ),
                                 bsCollapsePanel("Services", style = "warning", 
                                                 div(class="services",
                                                   fluidRow(
                                                     summaryBox("On-Campus day care for students' children", textOutput("stusrv8"),
                                                                icon = NULL, width = 6, style = "warning", border = "bottom"),
                                                     summaryBox("Placement services for completers", textOutput("stusrv4"), icon = NULL, width = 6,
                                                                border = "bottom", style = "warning")),
                                                   fluidRow(
                                                     summaryBox("Employment Services for Students", textOutput("stusrv3"), icon = NULL, width = 6,
                                                                border = "bottom", style = "warning"),
                                                     # tipify(#icon("question-circle", class = "profile_icon", lib = "font-awesome"),
                                                     #   textOutput("stusrv1"),
                                                     #        "Year: 2021; Undergraduate, full time students",
                                                     #        placement = NULL,
                                                     #        trigger = "hover")
                                                     summaryBox("Remedial Services", tipify(#icon("question-circle", class = "profile_icon", lib = "font-awesome"),
                                                       textOutput("stusrv1"),
                                                       "Remedial Services: Year 2021",
                                                       placement = NULL,
                                                       trigger = "hover"), icon = NULL, width = 6,
                                                                border = "bottom", style = "warning")
                                                     
                                                   ),
                                                   fluidRow(
                                                     
                                                     summaryBox("Academic/Career Counseling Service", textOutput("stusrv2"), 
                                                                width = 6, icon = NULL, border = "bottom", style = "warning"),
                                                     summaryBox("Weekend/Evening College", textOutput("slo7"), width = 6, icon = NULL,
                                                                border = "bottom", style = "warning")),
                                                   fluidRow(
                                                     summaryBox("Credit for Life Experiences", textOutput("credits2"), width = 6, icon = NULL,
                                                                border = "bottom", style = "warning"),
                                                     summaryBox("Undergraduate distance education programs offered", 
                                                                textOutput("dstnugp"), width = 6, icon = NULL, style = "warning", border = "bottom")
                                                     
                                                   ),
                                                 )),
                            
                                 
                                 bsCollapsePanel("State and National Comparisons", style = "warning",
                                                 
                                                   
                                                   # -----. EI
                                                  
                                                            br(),
                                                            # fluidRow(
                                                            #   style='background-color: #e9b73f; align: center; margin-left: 0px; position:sticky; top:40px;z-index:10',
                                                            #   column(12,
                                                            #          align = "left",
                                                            #          h5("Equity Indicators"))
                                                            # ),
                                                            div(
                                                              #style = "height: 210vh",
                                                              fluidRow(
                                                                br(),
                                                                navlistPanel(
                                                                  widths = c(2, 9),
                                                                  
                                                                  # -----.School Demographic Breakdown
                                                                  tabPanel("School Demographic Breakdown",
                                                                           style='margin-left:50px; height: 210vh;',
                                                                           fluidRow(style='background-color: #fff;  top:73px; z-index:10; margin-left:-15px',
                                                                                    align="left",
                                                                                    div(class="subtitlePanel",
                                                                                        "Undergraduate Enrollment Breakdown")),
                                                                           br(),
                                                                           br(),
                                                                           
                                                                           # -----. Pie charts
                                                                           fluidRow(style="margin-bottom:0px; width:1200px",
                                                                                    column(4, style = "margin-top: 0px; margin-right: -50px; margin-left: -10px;",
                                                                                           highchartOutput("pell_enroll_pie", height = 410)),
                                                                                    column(4, style = "margin-top: 0px; margin-left: 0px; margin-right: 0px;
                                                   border-right: 2px solid black; height: 280px; padding-top: 0px;",
                                                                                           highchartOutput("race_enroll_pie", height = 500)),
                                                                                    column(4, style = "margin-top: 0px; margin-left: 0px;",
                                                                                           highchartOutput("instructor_pie", height = 500)
                                                                                           #style = "border-left: 2px solid black; height: 300px;"
                                                                                    )
                                                                           ),
                                                                           br(),
                                                                           
                                                                           fluidRow(
                                                                             style = "margin-bottom: -50px; margin-top: 150px;",
                                                                             h3("Graduation Rates for Each Group Compared to the Overall Institution Graduation Rate"),
                                                                             style="width: 50%; height: auto; margin:0px",
                                                                             br(),
                                                                             div(class="subtitlePanel",
                                                                                 "Overall graduation rate at"),
                                                                             #h4("Overall graduation rate at"),
                                                                             h4(textOutput("institution_graduation")),
                                                                             highchartOutput("grand_compl_pie", width = 300, height = 180)
                                                                           ),
                                                                           br(),
                                                                           br(),
                                                                           
                                                                           fluidRow(style="width:1200px; margin-left:-100px; margin-top: -50px; margin-bottom: -50px;",
                                                                                    column(2, offset = 1, style='border-right: 1px solid black;',
                                                                                           h4("Pell"),
                                                                                           br(),
                                                                                           highchartOutput("pell_compl_pie",width = 180, height = 180),
                                                                                           h4("Non-Pell"),
                                                                                           br(),
                                                                                           highchartOutput("non_pell_compl_pie",width = 180, height = 180)
                                                                                    ),
                                                                                    column(9, style="margin:0px; padding:0px;",
                                                                                           fluidRow(style="margin:0px; padding:0px;",
                                                                                                    column(3, style="margin:0px; padding:0px;",
                                                                                                           h4("White"),
                                                                                                           br(),
                                                                                                           highchartOutput("white_compl_pie", width =180, height = 180)),
                                                                                                    column(3,style="margin:0px; padding:0px;",
                                                                                                           h4("Hispanic"),
                                                                                                           br(),
                                                                                                           highchartOutput("hispanic_compl_pie", width =180, height = 180)),
                                                                                                    column(3,style="margin:0px; padding:0px;",
                                                                                                           h4("American Indian and Alaska Native"),
                                                                                                           highchartOutput("aian_compl_pie", width =180, height = 180)),
                                                                                                    column(3,
                                                                                                           h4("Two or More Races"),
                                                                                                           br(),
                                                                                                           highchartOutput("two_or_more_compl_pie", width = 180, height = 180))
                                                                                           ),
                                                                                           fluidRow(#style = "margin-bottom: -50px; margin-top: -50px;",
                                                                                             column(3,
                                                                                                    h4("Black"),
                                                                                                    br(),
                                                                                                    highchartOutput("black_compl_pie", width =180, height = 180)),
                                                                                             column(3,
                                                                                                    h4("Asian"),
                                                                                                    br(),
                                                                                                    highchartOutput("asian_compl_pie", width =180, height = 180)),
                                                                                             column(3,
                                                                                                    h4("Native Hawaiian and Pacific Islander"),
                                                                                                    highchartOutput("nhpi_compl_pie", width =180, height = 180)),
                                                                                             column(3,
                                                                                                    h4("Race Unknown"),
                                                                                                    br(),
                                                                                                    highchartOutput("race_unknown_compl_pie", width = 180, height = 180)
                                                                                             )
                                                                                           )
                                                                                    )
                                                                           )
                                                                  ),
                                                                  tabPanel("Enrollment Percent",
                                                                       style='margin-left:85px;',
                                                                       fluidRow(highchartOutput("enrollment_bar"))
                                                                    
                                                                  ),
                                                                  tabPanel("Completion Racial Breakdown",
                                                                           style='margin-left:85px;',
                                                                           fluidRow(highchartOutput("completers_bar"))
                                                                  ),
                                                                  tabPanel("Instructor vs Student Races",
                                                                           style='margin-left:85px;',
                                                                           fluidRow(highchartOutput("instructor_enrollment_bar"))
                                                                  )
                                                                  # tabPanel("Enrollment vs Completion Rate",
                                                                  #          style='margin-left:85px;',
                                                                  #          fluidRow(highchartOutput("enrollment_completion_bar"))
                                                                  # )
                                                                  
                                                                  # -----. Navbar Menus
                                                                  # navbarMenu("Compare",
                                                                  #            
                                                                  #            tabPanel("Enrollment Percent",
                                                                  #                     style='margin-left:80px;',
                                                                  #                     fluidRow(highchartOutput("enrollment_bar"))
                                                                  #            ),
                                                                  #            
                                                                  #            tabPanel("Completion Racial Breakdown",
                                                                  #                     style='margin-left:80px;',
                                                                  #                     fluidRow(highchartOutput("completers_bar"))
                                                                  #            ),
                                                                  #            tabPanel("Instructor vs Student Races",
                                                                  #                     style='margin-left:80px;',
                                                                  #                     fluidRow(highchartOutput("instructor_enrollment_bar"))
                                                                  #            ),
                                                                  #            tabPanel("Enrollment vs Completion Rate",
                                                                  #                     style='margin-left:80px;',
                                                                  #                     fluidRow(highchartOutput("enrollment_completion_bar"))
                                                                  #            )
                                                                  # )
                                                                )
                                                              ))
                                                   
                                                 
                                 ), #end of collapse panel
                                 bsCollapsePanel("Institutional Comparisons", style = "warning",
                                                 
                                                 
                                                 # -----. Retention and Graduation
                                                 
                                                 br(),
                                                 
                                                
                                                 div(style = "height: 500px;",
                                                     fluidRow(
                                                       h3("Compare retention rates, graduation rates, transfer-out rates, enrollment make-up, and outcomes between institutions"),
                                                       br(),
                                                       h3("Select up to 3 Comparison Universities")
                                                     ),
                                                     fluidRow(
                                                       column(4,
                                                              selectizeInput(
                                                                inputId = "dname_comp",
                                                                label = " ",
                                                                #label = h3("Select up to 3 Comparisons Universities"),
                                                                choices = NULL,
                                                                multiple = FALSE,
                                                                selected = NULL,
                                                                options = list(placeholder = 'Select One'),
                                                                width="600px"
                                                              )),
                                                       column(4,
                                                              selectizeInput(
                                                                inputId = "dname2_comp",
                                                                label = " ",
                                                                #label = h3("Select a Comparison University"),
                                                                choices = NULL,
                                                                multiple = FALSE,
                                                                selected = NULL,
                                                                options = list(placeholder = 'Select One'),
                                                                width="600px"
                                                              )
                                                       ),
                                                       column(4,
                                                              selectizeInput(
                                                                inputId = "dname3_comp",
                                                                label = " ",
                                                                #label = h3("Select a Comparison University"),
                                                                choices = NULL,
                                                                multiple = FALSE,
                                                                selected = NULL,
                                                                options = list(placeholder = 'Select One'),
                                                                width="600px"
                                                              )
                                                       )),
                                                     br(),
                                                     br(),
                                                   fluidRow(
                                                navlistPanel(
                                                       widths = c(2, 9),
                                                       
                                                       # -----. Retention
                                                       
                                                       
                                                       tabPanel("Retention",
                                                                style='margin-left:50px',
                                                                
                                                                  
                                                                # fluidRow(style='background-color: #fff; top:73px; z-index:10',
                                                                #          align="center",
                                                                #          div(class="subtitlePanel",
                                                                #              "Full-Time Retention by Race")
                                                                # ),
                                                                fluidRow(
                                                                  div(class="subtitlePanel",
                                                                      "Retention Rate"),
                                                                  br(),
                                                                  h6("Part-time and full-time retention rates are the percent of the fall
                                                                     part-time/full-time cohort from the prior year that re-enrolled at 
                                                                     the institution as either full or part-time in the current year.")
                                                                ),
                                                                br(),
                                                               
                                                                fluidRow(
                                                                  #style = "height: 550px;",
                                                                  column(6,
                                                                         highchartOutput("retention")),
                                                                  column(6,
                                                                         highchartOutput("retention_comp"))
                                                                ),
                                                                fluidRow(
                                                                  column(6,
                                                                         highchartOutput("retention_comp2")),
                                                                  column(6,
                                                                         highchartOutput("retention_comp3"))
                                                                ),
                                                                # fluidRow(style='background-color: #fff; top:73px; z-index:10',
                                                                #          align="center",
                                                                #          div(class="subtitlePanel",
                                                                #              "Part-Time retention by race")
                                                                # ),
                                                                br()
                                                       ),

                                                       # -----. Navbar Menus
                                                       tabPanel("Graduation",
                                                                div(
                                                                style = 'margin-left:50px;',
                                                            
                                                                div(
                                                                fluidRow(
                                                                  div(class="subtitlePanel",
                                                                      "Graduation Rate"),
                                                                  br(),
                                                                  h6("The rate of students that attain a bachelor's degree
                                                                     within 150% of normal time.")
                                                                ),
                                                                br(),
                                                                fluidRow(
                                                                  column(6,
                                                                         highchartOutput("graduation")),
                                                                  column(6,
                                                                         highchartOutput("graduation_comp"))
                                                                ),
                                                                fluidRow(
                                                                  column(6,
                                                                         highchartOutput("graduation_comp2")),
                                                                  column(6,
                                                                         highchartOutput("graduation_comp3"))
                                                                )
                                                                )
                                                                
                                                                )),
                                                       tabPanel("Transfer-Out",
                                                                div(
                                                                  style = 'margin-left:50px;',
                                                                  # fluidRow(
                                                                  #   h3("Select up to 3 Comparison Universities")
                                                                  # ),
                                                                  # fluidRow(
                                                                  #   column(4,
                                                                  #          selectizeInput(
                                                                  #            inputId = "dname_transfer",
                                                                  #            label = " ",
                                                                  #            choices = NULL,
                                                                  #            multiple = TRUE,
                                                                  #            selected = NULL,
                                                                  #            options = list(placeholder = 'Select One'),
                                                                  #            width="600px"
                                                                  #          )),
                                                                  #   column(4,
                                                                  #          selectizeInput(
                                                                  #            inputId = "dname_transfer2",
                                                                  #            label = " ",
                                                                  #            choices = NULL,
                                                                  #            multiple = TRUE,
                                                                  #            selected = NULL,
                                                                  #            options = list(placeholder = 'Select One'),
                                                                  #            width="600px"
                                                                  #          )),
                                                                  #   column(4,
                                                                  #          selectizeInput(
                                                                  #            inputId = "dname_transfer3",
                                                                  #            label = " ",
                                                                  #            choices = NULL,
                                                                  #            multiple = TRUE,
                                                                  #            selected = NULL,
                                                                  #            options = list(placeholder = 'Select One'),
                                                                  #            width="600px"
                                                                  #          ))),
                                                                  div(
                                                                    fluidRow(
                                                                      div(class="subtitlePanel",
                                                                          "Transfer-Out Rate"),
                                                                      br(),
                                                                      h6("Total number of students who are known to have transferred out
                                                                         of the reporting institution within 150% of normal time to completion divided by the revised cohort.")
                                                                    ),
                                                                    br(),
                                                                    fluidRow(
                                                                      column(6,
                                                                             highchartOutput("transfer")),
                                                                      column(6,
                                                                             highchartOutput("transfer_comp"))
                                                                    ),
                                                                    fluidRow(
                                                                      column(6,
                                                                             highchartOutput("transfer_comp2")),
                                                                      column(6,
                                                                             highchartOutput("transfer_comp3"))
                                                                    )

                                                                  ))
                                                                ),
                                                       tabPanel("Enrollment",
                                                                style='margin-left:50px',
                                                                fluidRow(
                                                                  div(class="subtitlePanel",
                                                                      "Enrollment Make Up"),
                                                                  br(),
                                                                  h6("The racial make up of students enrolled at an institution each year")
                                                                ),
                                                                br(),
                                                                fluidRow(
                                                                  column(6,
                                                                         highchartOutput("enrollment")),
                                                                  column(6,
                                                                          highchartOutput("enrollment_comp"))
                                                                ),
                                                                fluidRow(
                                                                  column(6,
                                                                         highchartOutput("enrollment_comp2")),
                                                                  column(6,
                                                                         highchartOutput("enrollment_comp3"))
                                                                )
                                                       ),
                                                       tabPanel("Outcomes",
                                                                style='margin-left:50px',
                                                                fluidRow(
                                                                  div(class="subtitlePanel",
                                                                      "Outcomes"),
                                                                  br(),
                                                                  h6("2021 award and enrollment rates of degree/certificate-seeking undergraduate
                                                                      students who entered the institution by cohort and students receiving Pell Grants
                                                                      and students not receiving Pell Grants")
                                                                    ),
                                                                br(),
                                                                    fluidRow(
                                                                      column(6,
                                                                             highchartOutput("outcomes")),
                                                                      column(6,
                                                                             highchartOutput("outcomes_comp"))
                                                                    ),
                                                                    fluidRow(
                                                                      column(6,
                                                                             highchartOutput("outcomes_comp2")),
                                                                      column(6,
                                                                             highchartOutput("outcomes_comp3"))
                                                                    )
                                                       ),
                                                       
                                                     )
                                                   ))
                                                 
                                                 
                                 )
                                 # bsCollapsePanel("Outcomes Institution Comparisons", style = "warning",
                                 #                 fluidRow(
                                 #                   h3("Select up to 3 Comparison Universities")
                                 #                 ),
                                 #                 fluidRow(
                                 #                   column(4,
                                 #                          selectizeInput(
                                 #                            inputId = "dname_outcomes",
                                 #                            label = " ",
                                 #                            choices = NULL,
                                 #                            multiple = TRUE,
                                 #                            selected = NULL,
                                 #                            options = list(placeholder = 'Select One'),
                                 #                            width="600px"
                                 #                          )),
                                 #                   column(4,
                                 #                          selectizeInput(
                                 #                            inputId = "dname_outcomes2",
                                 #                            label = " ",
                                 #                            choices = NULL,
                                 #                            multiple = TRUE,
                                 #                            selected = NULL,
                                 #                            options = list(placeholder = 'Select One'),
                                 #                            width="600px"
                                 #                          )),
                                 #                   column(4,
                                 #                          selectizeInput(
                                 #                            inputId = "dname_outcomes3",
                                 #                            label = " ",
                                 #                            choices = NULL,
                                 #                            multiple = TRUE,
                                 #                            selected = NULL,
                                 #                            options = list(placeholder = 'Select One'),
                                 #                            width="600px"
                                 #                          ))),
                                                 # fluidRow(
                                                 #   div(class="subtitlePanel",
                                                 #       "Outcomes"),
                                                 #   br(),
                                                 #   h6("Award and enrollment rates of degree/certificate-seeking undergraduate
                                                 #      students who entered the institution by cohort and students receiving Pell Grants
                                                 #      and students not receiving Pell Grants")
                                                 # ),
                                                 # br(),
                                 #                 # fluidRow(
                                 #                 #   column(6,
                                 #                 #          highchartOutput("outcomes")),
                                 #                 #   column(6,
                                 #                 #          highchartOutput("outcomes_comp"))
                                 #                 # ),
                                 #                 # fluidRow(
                                 #                 #   column(6,
                                 #                 #          highchartOutput("outcomes_comp2")),
                                 #                 #   column(6,
                                 #                 #          highchartOutput("outcomes_comp3"))
                                 #                 # )
                                 #                 )
                      )
                    ),
                    
                    class = 'tab-panel',
                    
                    br(),
                    br(),
                    br(),
                    br(),
                    
                    #back to top -----
                    
                    fluidRow(
                      column(
                        width = 12,
                        align = 'center',
                        actionButton("toTop", "Back to Top",
                                     style = "material-flat",
                                     
                                     icon=icon("angle-double-up"))
                      )
                    ),
                    br(),
                    br(),
                    
                    #footer -------
                    
                    # fluidRow(
                    #   column(
                    #     3,
                    #     div(class = "footer",
                    #         "")
                    #   ),
                    #   column(
                    #     6,
                    #     align = "center",
                    #     div(class = "footer",
                    #         em("Scores range from -0.2 – 0.2 and are calculated through a series 
                    #            of subscores from weighted data points. Scores and subscores are 
                    #            calculated in relation to the financial standing of other schools 
                    #            within your sector. Read our methodology and find the glossary for 
                    #            the terms included in the index here."), style = "font-size:20px;",
                    #         style = "color:black")
                    #   )
                    # ),
                    br(),
                    # fluidRow(
                    #   column(
                    #     3,
                    #     div(class = "footer",
                    #         "")
                    #   ),
                    #   column(
                    #     6,
                    #     align = "center",
                    #     div(class = "footer",
                    #         em("Learn about the methodology behind the index, including the weight of 
                    #         piece of the index, at this link."), style = "font-size:20px;", style = "color:black")
                    #   )
                    # ),
                    br(),
                    br(),
                    fluidRow(
                      column(
                        3,
                        div(class = "footer",
                            "")
                      ),
                      column(
                        6, 
                        align = "left",
                        div(class = "footer",
                            "We invite your collaboration. Find our project on Github and add 
                        to our analysis. Reach out with ideas on data and topics you want 
                        to see addressed. We will be updating this dashboard regularly. 
                        Contact The MAPS Team at maps@sorensonimpact.com with comments, 
                        questions, or concerns.")
                        
                      )
                    ),
                    br(),
                    br(),
                    fluidRow(
                      column(
                        3,
                        div(class = "footer",
                            "")
                      ),
                      column(
                        6, 
                        align = "center",
                        div(class = "footer",
                            "Created by Sorenson Impact Center at the University of Utah")
                        
                      )
                    ),   
                    br(),
                    fluidRow(
                      column(
                        3,
                        div(class = "footer",
                            "")
                      ),
                      column(
                        6, 
                        align = "center",
                        img(src = "./images/logo.png", class = "logoImg")
                        
                      )
                    )
                    
)
















# define ui -------------------------------------------------------------------------------------------

ui <- fluidPage(
  #tags$head(
  #  tags$link(rel = "stylesheet", type = "text/css", href = "fhi_dash.css")
  #),
  useShinyjs(),
  extendShinyjs(text = "shinyjs.toTop = function() {window.scrollTo(0, 0)}", functions = "toTop"),
  theme = "fhi_dash.css",
  
  titlePanel(
    windowTitle = "MAPS Project",
    title = ""
  ),
  div(body, class= "fullpage")
)

# Parking lot ----------------------------------------------------------------------------------------

#          fluidRow(
#            column(5,
#                   h4("Equity Indicators"),
#                   highchartOutput("parallelOutput")),
#            column(7,
#                   h4("Equity Indicators Table"),
#                   reactableOutput("equityTable"))
#          ),
#          fluidRow(uiOutput("experiment"))
# ),
