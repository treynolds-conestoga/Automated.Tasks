#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinipsum
#' @import shinydashboard
#' @import ggplot2
#' @noRd
app_server <- function(input, output, session) {

  # Set Data
  TASKS <- reactive({
    get_tasks() %>%
      {
        if (input$broken_only_switch) {
          . |>
            dplyr::filter(`Last Result` == 'FAILED')
        } else{.}
      } |>
      dplyr::filter(HostName %in% input$vm_choice)
  })

  # Your application server logic
  # Data Table
  output$data_table <- DT::renderDT({
    #random_DT(5, 5)
    TASKS() |>
      dplyr::select(-`Task To Run`)
  }, options = list(pageLength = 5))

  # Plots
  output$troy_vm_distibution <- renderPlot({
    TASKS() %>%
      pie_plot(
        .DATA = .,
        .AUTHOR = "treynolds",
        .COLUMN = "HostName"
      )
  })
  output$ravi_vm_distibution <- renderPlot({
    TASKS() %>%
      pie_plot(
        .DATA = .,
        .AUTHOR = "rgoparaju",
        .COLUMN = "HostName"
      )
  })
  output$aaron_vm_distibution <- renderPlot({
    TASKS() %>%
      pie_plot(
        .DATA = .,
        .AUTHOR = "aknodell",
        .COLUMN = "HostName"
      )
  })
  output$troy_broken_percentage <- renderPlot({
    TASKS() %>%
      pie_plot(
        .DATA = .,
        .AUTHOR = "treynolds",
        .COLUMN = "Last Result"
      )
  })
  output$ravi_broken_percentage <- renderPlot({
    TASKS() %>%
      pie_plot(
        .DATA = .,
        .AUTHOR = "rgoparaju",
        .COLUMN = "Last Result"
      )
  })
  output$aaron_broken_percentage <- renderPlot({
    TASKS() %>%
      pie_plot(
        .DATA = .,
        .AUTHOR = "aknodell",
        .COLUMN = "Last Result"
      )
  })

  # Text
  output$text <- renderText({
    random_text(nwords = 50)
  })

  # Value Box output
  output$num_broken_tasks <- renderValueBox({
    valueBox(
      value =
        TASKS() |>
        get_num_broken_tasks(),
      subtitle = "Total Broken Tasks"
    )
  })
}
