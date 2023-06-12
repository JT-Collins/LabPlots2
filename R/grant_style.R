#' @export
grant_style <- function(colPal = c("uofl_col", "alt_col")) {

  # Check if required packages are installed and load the packages
  required_packages <- c("hrbrthemes", "ggplot2", "grid", "ggtext")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    install_commands <- paste("install.packages('", missing_packages, "')", sep = "")
    stop(paste("The following packages are required:", paste(missing_packages, collapse = ", "),
               "please install using", paste(install_commands, collapse = " or ")))
  }

  colPal <- match.arg(colPal)

  # Set color palette
  uofl_col <- c("#ad0000",
                              "#004e74",
                              "#8b9da1",
                              "#00a89d",
                              "#ffba00",
                              "#544009",
                              "#698900",
                              "#ddcb2e")

  alt_col <- c("#41afaa", "#466eb4", "#00a0e1", "#e6a532", "#d7642c", "#af4b91")

  dark_text <- "#1A242F"
  mid_text <- "#343D46"
  light_text <- "#4E565E"

  options(ggplot2.continuous.colour="viridis")
  options(ggplot2.continuous.fill = "viridis")

  if (colPal == "alt_col"){
  options(ggplot2.discrete.colour = alt_col )
  options(ggplot2.discrete.fill = alt_col )
  } else {
    options(ggplot2.discrete.colour = uofl_col )
    options(ggplot2.discrete.fill = uofl_col )

  }
  #hrbrthemes::import_roboto_condensed()


  # Set ggplot2 options and theme settings
  ggplot2::theme_set(
    hrbrthemes::theme_ipsum(
      grid = FALSE,
      #axis = 'yx',
      axis_col = "#5A5A5A",
      ticks = FALSE,
      base_size = 8,
      #plot_margin = ggplot2::margin(10, 5, 5, 10),
      plot_margin = ggplot2::margin(2, 2, 2, 2),
      #panel_spacing = grid::unit(1, "lines"),
      plot_title_size = 9,
      subtitle_size = 8,
      subtitle_margin = 5,
      axis_title_size = 9,
      strip_text_size = 8
          ) + theme(text = element_text(colour = mid_text),
                    plot.title = ggtext::element_markdown(color = dark_text, lineheight = 1.1),
                    plot.subtitle = ggtext::element_markdown(lineheight = 1.1),
                    #plot.title = element_text(colour = dark_text),
                    plot.title.position = "plot",
                    axis.text.y = element_text(colour = light_text),
                    axis.line=element_line(linewidth=0.3),
                    axis.text.x = element_text(colour = light_text, margin = margin(t = 3, b = 3)))
  )
  message(paste("ggtext is used to call plot.title and plot.subtitle enabling simple Markdown and HTML",
                "rendering for ggplot2. See https://wilkelab.org/ggtext/"))

  message(paste("If saving as a pdf file use 'device = cairo_pdf' to ensure correct font use"))
}
