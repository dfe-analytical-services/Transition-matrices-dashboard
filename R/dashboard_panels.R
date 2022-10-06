homepage_panel <- function() {
  tabPanel(
    "Homepage",
    gov_main_layout(
      gov_row(
        column(
          12,
          h1("Key stage 4 Transition Matrices"),
          br(),
          br()
        ),
        
        ## Left panel -------------------------------------------------------
        
        column(
          6,
          div(
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                h2("Contents")
              ),
              div(
                class = "panel-body",
                tags$div(
                  title = "This section is useful if you want to understand how well different industries retain graduates.",
                  h3("Introduction"),
                  p("This app demonstrates the KS4 Transition Matrices data dashboard."),
                  p("Transition matrices are a useful tool to help visualise the progression of pupils from key stage 2 (KS2) to key stage 4 (KS4) based on KS2 prior attainment scores and KS4 achievements found here:"),
                  p(actionLink("link_to_app_content_tab", "KS4 Transition Matrices dashboard")), 
                 # p(actionLink("link_to_app_content_tab", "KS4 Transition Matrices: KS4 headline measures (Academic year 2022)")),
                 # p("You need to add an observeEvent() function to the server.R script for any link that navigates within your App.")
                ),
                br(),
              )
            )
          ),
        ),
        
        ## Right panel ------------------------------------------------------
        
        column(
          6,
          div(
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                h2("Information")
              ),
              div(
                class = "panel-body",
                h3("Context and purpose"),
                p("To use the transition matrices the dashboard tab found on the left panel, choose a category from the tab panel, then select one option from each dropdown box. Figures are presented in the form of a table and chart. 
                   All underlying data can be downloaded in csv format from the Key stage 4 data catalogue."),                                          
                #p(" For example some useful links to your EES publication, data sources and other relevant resources."), # For example some useful links to your EES publication, data sources and other relevant resources.
                #p(actionLink("https://explore-education-statistics.service.gov.uk/data-catalogue/key-stage-4-performance-revised/2020-21", "KS4 Transition Matrices")), 
                
              
                
                h3("All supporting files"),
                p("All supporting files from this release are listed for individual download below:"),
                a(href = "https://explore-education-statistics.service.gov.uk/data-catalogue/key-stage-4-performance-revised/2020-21", "Key stage 4 data catalogue", .noWS = c("after")),
                br(),
                 a(href = "https://www.gov.uk/government/publications/key-stage-4-qualifications-discount-codes-and-point-scores", "Key stage 4 discounting and early entry guidance", .noWS = c("after")),
               br(),
                 a(href = "https://content.explore-education-statistics.service.gov.uk/api/releases/1c01df43-72e6-4ee4-a64e-2805f5e44d2d/files/dd9db45d-7f81-41a4-b147-08d88fd28587", "Key stage 4 GCSE Subject group composition ", .noWS = c("after")),
                
                
              )
            )
          )
        )
      )
    )
  )
  
}


dashboard_panel <- function() {
  tabPanel(
    value = "dashboard",
    "Dashboard",
    
    # Define UI for application that draws a histogram
    
    # Sidebar with a slider input for number of bins
    gov_main_layout(
      gov_row(
        column(
          width=12,
        h1("Key stage 4 Transition Matrices"),
       br(),#added
       br(),#added
         ),
       
        column(
          width=12,
          div(
            class = "well",
            style = "min-height: 100%; height: 1000%; overflow-y: visible",
            h2("Dashboard") #added
           

          )
        ),
        
  #######GCSE SUBJECTS tab#######  
        column(
          width=12,
               tabsetPanel(id = "tabsetpanels",
                
         tabPanel(
            "KS2-KS4 progress by GCSE subjects",
            fluidRow(
              column(
                width=12,
                h2("Summary"),
                p("The following data is based on KS2 prior attainment scores and KS4 achievements in GCSE subjects’ grades 9-1 for the 2021/2022 academic year. It has been broken down by pupil characteristics; disadvantage, English as an additional language (EAL), free school meal eligibility (FSM), special educational needs (SEN).
                           Figures are available at national (England) level only. Includes pupils in state-funded mainstream and special schools, hospital schools and non-maintained special schools."),
                
                column(
                  width=12,
                  div(
                    class = "well",
                    style = "min-height: 100%; height: 100%; overflow-y: visible",
                    fluidRow(
                      
            
                          
                      #h3("Select one option from all categories to explore data:"),
                      # column(6,selectInput(inputId = "num_perc_select",
                      #                      label = "1.Select one format",
                      #                      choices = num_perc_dropdown)),
                      
                      column(6,  selectInput(inputId = "characteristic_select",
                                             label = "2.Select one pupil characteristics",
                                             choices = characteristic_dropdown$characteristic_type)),
                      
                      column(6,selectInput(inputId = "subjects_select", 
                                           label = "3.Select one GCSE Subject",
                                           choices = subject_dropdown$subject)),   
                      
                      column(6, selectInput(inputId = "KS2_dropdown_attainment_subject",
                                            label = "4.Select one KS2 attainment score",
                                            choices = "" )),
                      
                        ),
                      )
                    )
                  ,
                  

                  column(
                    width=12,
                    # column(6,selectInput(inputId = "num_perc_select",
                    #                      label = "1.Select one format",
                    #                      choices = num_perc_dropdown)),
                    radioGroupButtons(inputId = "num_perc_select",
                                      label = "Select one format for both the chart and table(s):",
                                      choices = num_perc_dropdown),
                    box(
                      width=12,
                      htmlOutput('subjects_chart_title'),
                        plotlyOutput('subjects_chart')),
                      
                      column (12,
                              uiOutput("number_table"),
                      
                  ))))),
         
        

  
                  # fluidRow(
                   #  column(
                    #   width=12,
          #h2("Outputs 1"),
          #valueBoxOutput("boxavgRevBal", width = 6),
          #valueBoxOutput("boxpcRevBal", width = 6),
          #box(
           # width=12,
          #plotlyOutput("lineRevBal")))
        #)
        #),
        
        
 #######KS4 HEADLINE MEASURES tab#######    
 
        tabPanel(
          "KS2-KS4 progress by KS4 headline measures",
          fluidRow(
            column(
              width=12,
          h2("Summary"),
          p("The following data is based on KS2 prior attainment scores and KS4 achievements in EBacc entry, EBacc achievement (9-4), EBacc achievement (9-5),English and maths (9-4), English and maths (9-5) for the 2021/2022 academic year.
            It has been broken down by pupil characteristics; disadvantage, English as an additional language (EAL), free school meal eligibility (FSM), special educational needs (SEN). 
            Figures are available at national (England) level only.
            Includes pupils in state-funded mainstream and special schools, hospital schools and non-maintained special schools."),
     #   
       #     column(
      #      width=12,
        #    box(
         #     width=12,
          #    plotlyOutput("colBenchmark2")
           # )
          #),## continue here check above 
          column(
            width=12,
            div(
              class = "well",
              style = "min-height: 100%; height: 100%; overflow-y: visible",
              fluidRow(
                
              #  h3("Select one option from all categories to explore data:"),
                column(6, selectInput(inputId = "attainment_select",
                                      label = "1.Select one KS4 measure",
                                      choices = attainment_dropdown)),
                column(6, selectInput(inputId = "characteristic_att_select",
                                      label = "2.Select one pupil characteristic",
                                      choices = characteristic_dropdown$characteristic_type)),
                column(6,  selectInput(inputId = "KS2_att_select",
                                       label = "3.Select one KS2 attainment score",
                                       choices = KS2_dropdown_attainment)),
     
              ),
            )
          )
          ,
          
         
          
          column(
            width=12,
            box(
              width=12,
              radioGroupButtons('num_perc',
                                label = "Select one format for both the chart and table(s):",
                                choices = c('Number', 'Percentage')),
              p("Key stage 2 to Key stage 4 pupil progress in KS4 headline measures", style = "font-size:20px;"),
              conditionalPanel("input.num_perc == 'Number'",
                               plotlyOutput('attainment_chart_num')),
              conditionalPanel("input.num_perc == 'Percentage'",
                               plotlyOutput('attainment_chart_perc')),
              ),
              
              column (12,
                      uiOutput('attainment_table'),
                      
            ))),))))
 
 
 

 
 
       #   ))
        #))
         #     )
      #  )
        # add box to show user input
)
          )
  )
}
