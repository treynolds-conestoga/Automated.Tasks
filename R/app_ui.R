#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinyjs
#' @import shinyWidgets
#' @import vrtoolbox
#' @noRd
app_ui <- function(request) {

  LIST_OF_VMS <- get_tasks() |>
    dplyr::pull(HostName) |>
    unique()

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      title = "Automated Tasks",
      header = dashboardHeader(
        title = div(img(src = "www/Conestoga-white.png", style = "width: 100%; height: auto;"))
      ),
      sidebar = dashboardSidebar(
        sidebarMenu(
          id = "sidebar_menu",
          menuItem(
            text = "Automated Task Tracker",
            tabName = "automated_task_tracker"
          )
        )
      ),
      body = dashboardBody(
        tags$script(HTML("$('body').addClass('fixed');")),
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "./inst/app/www/vr_css_styling.css")
        ),
        useShinyjs(),
        tabItems(

          # Inventory Viewer
          tabItem(
            tabName = "automated_task_tracker",
            # Controls
            fluidRow(
              box(
                title = "Task Viewer Settings",
                width = 12,
                fluidRow(
                  pickerInput(
                    inputId = "vm_choice",
                    label = div(
                      shiny::tags$label(
                        class = "control-label",
                        "Select which VMs to view"),
                      br()
                    ),
                    choices = LIST_OF_VMS,
                    selected = LIST_OF_VMS,
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
                  ) |> column(width = 4),
                  shinyWidgets::materialSwitch(
                    inputId = "broken_only_switch",
                    label = div(
                      shiny::tags$label(
                        class = "control-label",
                        "Only Show Broken Tasks"),
                      br()
                    )
                  ) |> column(width = 4),
                  valueBoxOutput("num_broken_tasks", width = 4)
                )
              )
            ),
            # Pie charts
            fluidRow(
              box(
                title = "VM Distribution",
                width = 12,
                fluidRow(
                  h3("Troy") |> column(width = 4),
                  h3("Ravi") |> column(width = 4),
                  h3("Aaron") |> column(width = 4)
                ) |> column(width = 12),
                fluidRow(
                  plotOutput("troy_vm_distibution") |>  column(width = 4),
                  plotOutput("ravi_vm_distibution") |>  column(width = 4),
                  plotOutput("aaron_vm_distibution") |>  column(width = 4)
                ) |> column(width = 12)
              )
            ),
            # Pie Charts
            fluidRow(
              box(
                title = "Broken Task Percentage",
                width = 12,
                fluidRow(
                  h3("Troy") |> column(width = 4),
                  h3("Ravi") |> column(width = 4),
                  h3("Aaron") |> column(width = 4)
                ) |> column(width = 12),
                fluidRow(
                  plotOutput("troy_broken_percentage") |>  column(width = 4),
                  plotOutput("ravi_broken_percentage") |>  column(width = 4),
                  plotOutput("aaron_broken_percentage") |>  column(width = 4)
                ) |> column(width = 12)
              )
            ),
            # Data Table
            fluidRow(
              DT::DTOutput("data_table") |> column(width = 12)
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Automated.Tasks"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
