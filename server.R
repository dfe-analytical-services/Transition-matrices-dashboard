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

server <- function(input, output, session) {
  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- Reactive KS2 attainment level from subject, numperc, and characteristic drop-down selections ----
  # -----------------------------------------------------------------------------------------------------------------------------

  KS2_prior_subj <- reactive({
    if (input$subjects_select == "Combined Science") {
      download_Combined_Science_data %>%
        filter(characteristic_type == input$characteristic_select) %>%
        select(KS2_Prior) %>%
        distinct() %>%
        #  arrange(KS2_Prior) %>%
        unlist(use.names = FALSE)
    } else {
      download_GCSE_Subjects_data %>%
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
      
    )
    
  })


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
    tables <- map(numbers_data_split(), ~ {
      renderDataTable({
        datatable(
          (.x %>%
            rename("KS2 Attainment" = "KS2_Prior") %>%
            select(-"characteristic_value")),
          caption = htmltools::tags$caption(as.character(.x[1, "characteristic_value"]), style = "color: #104f75 ; font-size:16pt"),
          # caption = as.character(.x[1, "characteristic_value"]),
          options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), bFilter = FALSE, bPaginate = FALSE, scrollX = TRUE),
          rownames = FALSE
        )
      })


      # formatStyle('KS2 Attainment', target = 'row',
      #            backgroundColor = styleEqual('KS2_dropdown_attainment', '#D4CEDE')
    })
  })

  # create the output attainment table
  output$attainment_table <- renderUI({
    tables <- map(attainment_data_split(), ~ {
      renderDataTable({
        datatable(
          (.x %>%
            rename("KS2 Attainment" = "KS2 Prior") %>%
            select(-"characteristic_value")),
          caption = htmltools::tags$caption(as.character(.x[1, "characteristic_value"]), style = "color: #104f75 ; font-size:16pt"),
          options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), bFilter = FALSE, bPaginate = FALSE, scrollX = TRUE),
          rownames = FALSE
        )
      })
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

  output$subjects_chart_title <- renderText(
    paste("<h4> KS2-KS4 pupil progress in GCSE ", input$subjects_select, ", with ", input$KS2_dropdown_attainment_subject, " KS2 scaled score </43>",
      sep = ""
    )
  )


  output$subjects_chart <- renderPlotly(
    {
      chart_data <- numbers_data() %>%
        filter(KS2_Prior == input$KS2_dropdown_attainment_subject) %>%
        rename(Characteristic = characteristic_value)


      if (input$num_perc_select == "Number of pupils") {
        chart_data <- chart_data %>%
          select(-"All Grades")
      }


      chart_data <- reshape2::melt(chart_data)
      subjects_chart <- ggplot(chart_data) +
        (aes(x = variable, y = value, fill = Characteristic)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("#12436D", "#28A197")) +
        xlab("GCSE Grades") +

        # ggtitle("Key stage 2 to Key stage 4 pupil progress in GCSE subjects")+
        scale_y_continuous(
          name = paste(input$num_perc_select),
          expand = c(0, 0),
          breaks = function(x) {
            unique(floor(pretty(seq(0, max(x) + 1) * 1.1)))
          },
          limits = function(x) {
            c(0, (max(x) + 1) * 1.1)
          }
        ) +
        # scale_x_discrete(labels =  c("U", "1", "2", "3", "4", "5", "6", "7", "8", "9", "X", "covid impacted \n"))
        #  scale_x_discrete(labels = c("U", "11", "21", "22", "32", "33", "43", "44", "54", "55", "65", "66", "76", "77", "87", "88", "98", "99", "X", "covid_impacted"))



        scale_x_discrete(labels = function(x) {
          stringr::str_wrap(x, width = 10)
        })



      # scale_x_discrete(labels = c(chart_data$variable[1:length(chart_data$variable)-1],"Covid\nimpacted")) +
      # scale_x_discrete(labels = str_wrap(labels, 10))
      theme(
        # set size and spacing of axis tick labels
        axis.text.x = element_text(size = 12, vjust = 0.5),
        axis.text.y = element_text(size = 12, vjust = 0.5),
        # set size, colour and spacing of axis labels
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = 2.0),
        # sorting out the background colour, grid lines, and axis lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "White"),
        plot.background = element_rect(fill = "White", color = NA),
        axis.line = element_line(colour = "black"),
        legend.position = "top"
      )



      ggplotly(subjects_chart) %>%
        config(displayModeBar = F) %>%
        layout(legend = list(orientation = "h", y = -0.1))
    } # ,
    # bg = 'transparent'
  )
  #####################

  ######################

  output$attainment_chart_title <- renderText(
    paste("<h4> KS2-KS4 pupil progress in ", input$attainment_select, ", with ", input$KS2_att_select, " KS2 scaled score </43>",
      sep = ""
    )
  )


  output$attainment_chart_num <- renderPlotly({
    num_chart_data <- attainment_data() %>%
      filter(`KS2 Prior` == input$KS2_att_select) %>%
      select(-starts_with("% ")) %>%
      rename(Characteristic = characteristic_value)


    num_chart_data <- reshape2::melt(num_chart_data)

    number_plot <- ggplot(num_chart_data, aes(x = variable, y = value, fill = Characteristic, group = Characteristic)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("#12436D", "#28A197")) +
      xlab(" ") +
      # ggtitle("Key stage 2 to Key stage 4 pupil progress in KS4 headline measures")+
      scale_y_continuous(
        name = paste(input$num_perc, "of pupils"),
        expand = c(0, 0),
        breaks = function(x) {
          unique(floor(pretty(seq(0, max(x) + 1) * 1.1)))
        },
        limits = function(x) {
          c(0, (max(x) + 1) * 1.1)
        }
      ) +
      theme(
        # set size and spacing of axis tick labels
        axis.text.x = element_text(size = 12, vjust = 0.5),
        axis.text.y = element_text(size = 12, vjust = 0.5),
        # set size, colour and spacing of axis labels
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = 2.0),
        # sorting out the background colour, grid lines, and axis lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "White"),
        plot.background = element_rect(fill = "White", color = NA),
        axis.line = element_line(colour = "black"),
        legend.position = "top"
      )

    ggplotly(number_plot) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(orientation = "h", y = -0.1))
  })

  output$attainment_chart_perc <- renderPlotly({
    perc_chart_data <- attainment_data() %>%
      filter(`KS2 Prior` == input$KS2_att_select) %>%
      select(`KS2 Prior`, characteristic_value, starts_with("% ")) %>%
      rename(Characteristic = characteristic_value)

    perc_chart_data <- reshape2::melt(perc_chart_data)

    perc_plot <- ggplot(perc_chart_data, aes(x = variable, y = value, fill = Characteristic)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("#12436D", "#28A197")) +
      xlab(" ") +
      # ggtitle("Key stage 2 to Key stage 4 pupil progress in KS4 headline measures")+
      scale_y_continuous(
        name = paste(input$num_perc, "of pupils"),
        expand = c(0, 0),
        breaks = function(x) {
          unique(floor(pretty(seq(0, max(x) + 1) * 1.1)))
        },
        limits = function(x) {
          c(0, (max(x) + 1) * 1.1)
        }
      ) +
      theme(
        # set size and spacing of axis tick labels
        axis.text.x = element_text(size = 12, vjust = 0.5),
        axis.text.y = element_text(size = 12, vjust = 0.5),
        # set size, colour and spacing of axis labels
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = 2.0),
        # sorting out the background colour, grid lines, and axis lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "White"),
        plot.background = element_rect(fill = "White", color = NA),
        axis.line = element_line(colour = "black"),
        legend.position = "top"
      )

    ggplotly(perc_plot) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(orientation = "h", y = -0.1))
  })

  # -----------------------------------------------------------------------------------------------------------------------------
  # ---- Download Buttons ----
  # -----------------------------------------------------------------------------------------------------------------------------



  output$GCSE_Subjects_data_download <- downloadHandler(
    filename = "2023_KS4_GCSE_Subjects_TM_data.csv",
    content = function(file) {
      write.csv(download_GCSE_Subjects_data, file, row.names = FALSE)
    }
  )

  output$Combined_Science_data_download <- downloadHandler(
    filename = "2023_KS4_Combined_Science_TM_data.csv",
    content = function(file) {
      write.csv(download_Combined_Science_data, file, row.names = FALSE)
    }
  )

  output$attainment_data_download <- downloadHandler(
    filename = "2023_KS4_meaures_TM_data.csv",
    content = function(file) {
      write.csv(download_attainment_data, file, row.names = FALSE)
    }
  )



  #  output$download_user_subject_table <- downloadHandler(
  #   filename = "subject_data.csv",
  #  content = function(file) {
  #   write.csv(numbers_data(), file, row.names = FALSE)
  #  }
  # )

  # output$download_user_attainments_table <- downloadHandler(
  # filename = "attainment_data.csv",
  # content = function(file) {
  # write.csv(attainment_data(), file, row.names = FALSE)
  # }
  #  )
}
