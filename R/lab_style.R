#' @export
lab_style <- function() {

  # Check if required packages are installed and load the packages
  required_packages <- c("hrbrthemes", "ggplot2", "grid", "ggtext")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    install_commands <- paste("install.packages('", missing_packages, "')", sep = "")
    stop(paste("The following packages are required:", paste(missing_packages, collapse = ", "),
               "please install using", paste(install_commands, collapse = " or ")))
  }

  # Set color palette
  uofl_col <- c("#ad0000",
                "#004e74",
                "#8b9da1",
                "#00a89d",
                "#ffba00",
                "#544009",
                "#698900",
                "#ddcb2e")

  dark_text <- "#343D46"
  mid_text <- "#4E565E"
  light_text <- "#686F76"

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
      strip_text_size = 11

    ) + theme(text = ggplot2::element_text(colour = mid_text),
              axis.text.x = ggplot2::element_text(vjust = -1,colour = light_text),
              axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 1), colour = light_text),
              plot.title = ggtext::element_markdown(color = dark_text, lineheight = 1.1),
              plot.subtitle = ggtext::element_markdown(lineheight = 1.1),
              #plot.title = ggplot2::element_text(color = dark_text),
              plot.title.position = "plot",
              axis.line = ggplot2::element_line(colour = light_text, linewidth = 0.5),
              axis.ticks.length = ggplot2::unit(3, "pt"))
  )

message(paste("ggtext is used to call plot.title and plot.subtitle enabling simple Markdown and HTML",
      "rendering for ggplot2. See https://wilkelab.org/ggtext/"))

message(paste("If saving as a pdf file use 'device = cairo_pdf' to ensure correct font use"))

}
