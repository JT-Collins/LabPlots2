#' @export
grant_style <- function(col = "uofl_col") {

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
  alt_col <- c("#41afaa", "#466eb4", "#00a0e1", "#e6a532", "#d7642c", "#af4b91")

  options(ggplot2.continuous.colour="viridis")
  options(ggplot2.continuous.fill = "viridis")
  options(ggplot2.discrete.colour = col )
  options(ggplot2.discrete.fill = col )

  #hrbrthemes::import_roboto_condensed()


  # Set ggplot2 options and theme settings
  ggplot2::theme_set(
    hrbrthemes::theme_ipsum(
      grid = FALSE,
      #axis = 'yx',
      axis_col = "#5A5A5A",
      ticks = FALSE,
      base_size = 8,
      plot_margin = ggplot2::margin(10, 5, 5, 10),
      #panel_spacing = grid::unit(1, "lines"),
      plot_title_size = 9,
      subtitle_size = 8,
      axis_title_size = 9,
      strip_text_size = 8
          ) + theme(axis.line=element_line(linewidth=0.3))
  )

}
