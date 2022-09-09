# ---------------------------------------------------------
# This is the server file.
# Use it to create interactive elements like tables, charts and text for your app.
#
# Anything you create in the server file won't appear in your app until you call it in the UI file.
# This server script gives an example of a plot and value box that updates on slider input.
# There are many other elements you can add in too, and you can play around with their reactivity.
# The "outputs" section of the shiny cheatsheet has a few examples of render calls you can use:
# https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
#
# This is the server logic of a Shiny web application. You can run th
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------
library(cowplot)

server <- function(input, output, session) {
  
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- Reactive KS2 attainment level from subject, numperc, and characteristic drop-down selections ----
  # -----------------------------------------------------------------------------------------------------------------------------
  
  KS2_prior_subj = reactive({
    if(input$subjects_select == "Combined Science"){
      cs_data %>%
        filter(characteristic_type == input$characteristic_select) %>%
        select(KS2_Prior) %>%
        distinct() %>%
      #  arrange(KS2_Prior) %>% 
      unlist(use.names = FALSE)
    }
    
    else{
      subject_data %>%
        filter(subjects == input$subjects_select) %>%
        filter(characteristic_type == input$characteristic_select) %>%
        select(KS2_Prior) %>%
        distinct() %>%
        #  arrange(KS2_Prior) %>% 
        unlist(use.names = FALSE)
    }
  })
  
  
  observe({
    updateSelectInput(session, "KS2_dropdown_attainment_subject",
                      choices = KS2_prior_subj()
    )})
  
  
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- Re-active tables from drop-down selections ----
  # -----------------------------------------------------------------------------------------------------------------------------
  
  ## at this point, these tables contain a characteristic type column
  
  # build a reactive table for numbers table
  numbers_data <- reactive({
    subject_table(input$subjects_select, input$characteristic_select, input$num_perc_select)
  })
  
  
  # build a reactive table for attainment table
  attainment_data <- reactive({
    attainment_table(input$attainment_select, input$characteristic_att_select)
  })
  
  
  
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- Split on characteristics type ----
  # -----------------------------------------------------------------------------------------------------------------------------
  # 
  
  numbers_data_split <- reactive({
    split(numbers_data(), numbers_data()$characteristic_value)
  })
  
  
  
  attainment_data_split <- reactive({
    split(attainment_data(), attainment_data()$characteristic_value)
  })
  
  
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- Creating output tables ----
  # -----------------------------------------------------------------------------------------------------------------------------
  
  # create the output number table
  output$number_table <- renderUI({
    
    tables <- map(numbers_data_split() , ~{
      renderDataTable({.x %>%
          rename("KS2 Attainment" = "KS2_Prior") %>%
          select(-"characteristic_value")},
          caption = htmltools::tags$caption(as.character(.x[1, "characteristic_value"]), style="color: #104f75 ; font-size:16pt"),
          # caption = as.character(.x[1, "characteristic_value"]),
          options = list(columnDefs = list(list(className = 'dt-center', targets = '_all')), bFilter = FALSE, bPaginate = FALSE, scrollX = TRUE),
          rownames = FALSE)
      
      
      # formatStyle('KS2 Attainment', target = 'row',
      #            backgroundColor = styleEqual('KS2_dropdown_attainment', '#D4CEDE')
    })
  })
  
  # create the output attainment table
  output$attainment_table <- renderUI({
    
    tables <- map(attainment_data_split() , ~{
      renderDataTable({.x %>%
          rename("KS2 Attainment" = "KS2 Prior") %>%
          select(-"characteristic_value")},
          caption = htmltools::tags$caption(as.character(.x[1, "characteristic_value"]), style="color: #104f75 ; font-size:16pt"),
          options = list(columnDefs = list(list(className = 'dt-center', targets = '_all')), bFilter = FALSE, bPaginate = FALSE, scrollX = TRUE),
          rownames = FALSE)     
      
      
      
    })
  })
  
  
  
  
  # create example tables
#  output$example_table <- DT::renderDataTable({datatable(
 #   example_data %>% select(-c("characteristic_value", "X", "All Grades")),
  #  options = list(columnDefs = list(list(className = 'dt-center', targets = '_all')), bFilter = FALSE, bPaginate = FALSE, scrollX = TRUE),
   # rownames = FALSE) %>% 
    #  formatStyle('6', 'KS2 Attainment',
     #             backgroundColor = styleEqual('110 - 113', '#D4CEDE')
    #  )
#  })
  
#  output$example_table_perc <- DT::renderDataTable({datatable(
#    example_data_perc %>% select(-"characteristic_value"),
#    options = list(columnDefs = list(list(className = 'dt-center', targets = '_all')), bFilter = FALSE, bPaginate = FALSE, scrollX = TRUE),
#    rownames = FALSE) %>%
 #     formatStyle('% 6', 'KS2 Attainment', 
  #                backgroundColor = styleEqual('110 - 113', '#D4CEDE')
#   #   )
 # })
  
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- Creating output charts ----
  # -----------------------------------------------------------------------------------------------------------------------------
  
  output$subjects_chart = renderPlot({
    
    chart_data <- numbers_data() %>%
      filter(KS2_Prior == input$KS2_dropdown_attainment_subject) %>% 
      rename(Characteristic = characteristic_value)
    
    if(input$num_perc_select == 'Number of pupils'){
      chart_data <- chart_data %>%
        select(-"All Grades")
    }
    
    
    chart_data <- reshape2::melt(chart_data)
    ggplot(chart_data) +
      (aes(x = variable, y = value, fill = Characteristic)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      scale_fill_manual(values = c('#9FB9C8', '#A89CBD')) +
      xlab('Grades') +
      scale_y_continuous(name = paste(input$num_perc_select, "\n", 'with', input$KS2_dropdown_attainment_subject, 'KS2 prior attainment', sep = " "),
                         expand = c(0, 0)) + 
      theme(
        # set size and spacing of axis tick labels
        axis.text.x=element_text(size=15, vjust=0.5),
        axis.text.y=element_text(size=15, vjust=0.5),
        # set size, colour and spacing of axis labels
        axis.title.x = element_text(size=15, vjust=-0.5),
        axis.title.y = element_text(size=15, vjust=2.0),
        # sorting out the background colour, grid lines, and axis lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'White'),
        plot.background = element_rect(fill = 'White', color = NA),
        axis.line = element_line(colour = 'black')
      )
  },
  bg = 'transparent'
  )
  
  
  output$attainment_chart = renderPlot({
    
    num_chart_data <- attainment_data() %>%
      filter(`KS2 Prior` == input$KS2_att_select) %>%
      select(-starts_with("% ")) %>% 
      rename(Characteristic = characteristic_value)
    
    
    num_chart_data <- reshape2::melt(num_chart_data)
    
    
    perc_chart_data <- attainment_data() %>%
      filter(`KS2 Prior` == input$KS2_att_select) %>%
      select(`KS2 Prior`, characteristic_value, starts_with("% ")) %>% 
      rename(Characteristic = characteristic_value)
    
    perc_chart_data <- reshape2::melt(perc_chart_data)
    
    
    
    number_plot <- ggplot(num_chart_data, aes(x = variable, y = value, fill = Characteristic, group = Characteristic)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      scale_fill_manual(values = c('#9FB9C8', '#A89CBD')) +
      xlab(' ') +
      scale_y_continuous(name = paste(input$attainment_select, 'numbers', "\n", 'with ', input$KS2_att_select, 'KS2 prior attainment', sep = " "),
                         expand = c(0, 0)) + 
      theme(
        # set size and spacing of axis tick labels
        axis.text.x=element_text(size=15, vjust=0.5),
        axis.text.y=element_text(size=15, vjust=0.5),
        # set size, colour and spacing of axis labels
        axis.title.x = element_text(size=15, vjust=-0.5),
        axis.title.y = element_text(size=15, vjust=2.0),
        # sorting out the background colour, grid lines, and axis lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'White'),
        plot.background = element_rect(fill = 'White', color = NA),
        axis.line = element_line(colour = 'black')
      )
    
    perc_plot <- ggplot(perc_chart_data, aes(x = variable, y = value, fill = Characteristic)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      scale_fill_manual(values = c('#9FB9C8', '#A89CBD')) +
      xlab(' ') +
      scale_y_continuous(name = paste(input$attainment_select, 'percentages', "\n", 'with', input$KS2_att_select, 'KS2 prior attainment', sep = " "),
                         expand = c(0, 0)) + 
      theme(
        # set size and spacing of axis tick labels
        axis.text.x=element_text(size=15, vjust=0.5),
        axis.text.y=element_text(size=15, vjust=0.5),
        # set size, colour and spacing of axis labels
        axis.title.x = element_text(size=15, vjust=-0.5),
        axis.title.y = element_text(size=15, vjust=2.0),
        # sorting out the background colour, grid lines, and axis lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'White'),
        plot.background = element_rect(fill = 'White', color = NA),
        axis.line = element_line(colour = 'black')
      )
    
    plot_grid(number_plot, perc_plot, labels = " ")
    
  },
  bg = 'transparent'
  )
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- Download Buttons ----
  # -----------------------------------------------------------------------------------------------------------------------------
  
  
  # 
  output$download_tm_subject <- downloadHandler(
    filename = "KS4_TM_subject_data.csv",
   content = function(file) {
      write.csv(subject_data, file, row.names = FALSE)
    })  
  
  output$download_tm_combined_science <- downloadHandler(
    filename = "KS4_TM_combined_science_data.csv",
    content = function(file) {
      write.csv(cs_data, file, row.names = FALSE)
    })  
  
  output$download_tm_attainment <- downloadHandler(
    filename = "KS4_TM_attainment_data.csv",
    content = function(file) {
      write.csv(attainment_data, file, row.names = FALSE)
    })  
  
  
  
  output$download_user_subject_table <- downloadHandler(
    filename = "subject_data.csv",
    content = function(file) {
      write.csv(numbers_data(), file, row.names = FALSE)
    })  
  
 output$download_user_attainment_table <- downloadHandler(
    filename = "attainment_data.csv",
    content = function(file) {
      write.csv(attainment_data(), file, row.names = FALSE)
    })  
  
  
}







