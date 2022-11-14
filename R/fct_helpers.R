#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
pie_plot <- function(.DATA, .AUTHOR, .COLUMN) {

  # Transform Data
  DATA <- .DATA |>
    # Only get desired authors
    dplyr::filter(Author %in% .AUTHOR) |>

    # Get breakdown of desired column
    dplyr::group_by(across(all_of(.COLUMN))) |>
    dplyr::summarise(num = dplyr::n()) |>

    # Mutate to get lables
    dplyr::mutate(
      csum = rev(cumsum(rev(num))),
      pos = num/2 + dplyr::lead(csum, 1),
      pos = ifelse(is.na(pos), num/2, pos)
    )

  # Plot
  if(nrow(DATA) > 0 & !is.null(DATA)) {
    DATA |>
      # Plot the pie chart
      ggplot(aes(x = '', y = num, fill = get(.COLUMN))) +
      geom_bar(stat="identity", width=1) +
      scale_fill_discrete(name = .COLUMN) +
      coord_polar("y", start=0) +
      geom_text(aes(label = num), position = position_stack(vjust = 0.5), size = 6) +
      guides(fill = guide_legend(title = "Group")) +
      scale_y_continuous(breaks = DATA$pos, labels = DATA[[.COLUMN]]) +
      theme(axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text = element_text(size = 10, face = "bold"),
            legend.position = "none", # Removes the legend
            panel.background = element_rect(fill = "white"))
  } else{return(NULL)}
}
