#' @export
lab_style <- function() {

  # Check if required packages are installed and load the packages
  required_packages <- c("hrbrthemes", "ggplot2", "grid")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    install_commands <- paste("install.packages('", missing_packages, "')", sep = "")
    stop(paste("The following packages are required:", paste(missing_packages, collapse = ", "),
               "please install using", paste(install_commands, collapse = " or ")))
  }

  # Set color palette
  uofl_col <- c("#8B9DA1", "#AD0000", "#004E74", "#FEBE10" ,"#00A89D","#7A6C53",  "#AAB43A", "#D9C982")

  options(ggplot2.continuous.colour="viridis")
  options(ggplot2.continuous.fill = "viridis")
  options(ggplot2.discrete.colour = uofl_col )
  options(ggplot2.discrete.fill = uofl_col )

  # Import Roboto Condensed font
  hrbrthemes::import_roboto_condensed()

  ggplot2::theme_set(
    hrbrthemes::theme_ipsum_rc(
      grid = FALSE,
      axis = FALSE,
      ticks = TRUE,
      base_size = 10,
      plot_margin = ggplot2::margin(10, 5, 5, 10),
      panel_spacing = grid::unit(1, "lines"),
      plot_title_size = 16,
      subtitle_size = 14,
      axis_title_size = 14,
      strip_text_size = 11,
      axis.text.x = ggplot2::element_text(vjust = -1),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 1)),
      plot.title = ggplot2::element_text(color = "grey30"),
      plot.title.position = "plot",
      axis.line = ggplot2::element_line(colour = "grey20", size = 0.5),
      axis.ticks.length = ggplot2::unit(3, "pt"),
      axis.text = ggplot2::element_text(color = "grey20")
    )
  )

}
