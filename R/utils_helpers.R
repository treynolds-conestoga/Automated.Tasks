#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
get_tasks <- function() {
  vrtoolbox::sql.initialize_connector("data_science") %>%
    vrtoolbox::sql.run_query(
      .CON = .,
      .QUERY = "select * from scheduled_tasks"
    )
}

get_num_broken_tasks <- function(.DATA) {
  .DATA |>
    dplyr::filter(`Last Result` != "SUCCESS") |>
    nrow()
}
