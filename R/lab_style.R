#' @export
lab_style <- function(colPal = c("uofl_col", "alt_col")) {

  # Check if required packages are installed and load the packages
  required_packages <- c("hrbrthemes", "ggplot2", "grid", "ggtext")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    install_commands <- paste("install.packages('", missing_packages, "')", sep = "")
    stop(paste("The following packages are required:", paste(missing_packages, collapse = ", "),
               "please install using", paste(install_commands, collapse = " or ")))
  }

  colPal <- match.arg(colPal)

  options(ggplot2.continuous.colour = "viridis")
  options(ggplot2.continuous.fill = "viridis")

  if (colPal == "pal2") {
    options(ggplot2.discrete.colour = pal2 )
    options(ggplot2.discrete.fill = pal2 )
  } else if (colPal == "pal3") {
    options(ggplot2.discrete.colour = pal3 )
    options(ggplot2.discrete.fill = pal3 )

  } else {
    options(ggplot2.discrete.colour = pal1 )
    options(ggplot2.discrete.fill = pal1 )

  }

  # Import public sans Condensed font
  #hrbrthemes::import_public_sans()

  ggplot2::theme_set(
    hrbrthemes::theme_ipsum(
      grid = FALSE,
      axis = TRUE,
      ticks = TRUE,
      axis_col = "black",
      base_size = 12,
      plot_margin = ggplot2::margin(15, 10, 10, 15),
      plot_title_size = 16,
      subtitle_size = 14,
      axis_title_size = 14,
      strip_text_size = 14

    ) + theme(text = ggplot2::element_text(colour = mid_text),
              axis.text.y = element_text(colour = black_text, margin = margin(r = 0.8), size = 12),
              axis.text.x = element_text(colour = black_text, margin = margin(t = 0.8), size = 12),
              axis.title.x = element_text(vjust = -0.75),
              plot.title = ggtext::element_markdown(color = dark_text, lineheight = 1.1),
              plot.subtitle = ggtext::element_markdown(lineheight = 1.1),
              strip.text = element_text(colour = dark_text),
              #plot.title = ggplot2::element_text(color = dark_text),
              plot.title.position = "plot",
              panel.spacing = grid::unit(1, "lines"),
              axis.line = ggplot2::element_line(colour = mid_text,
                                                lineend = 'square',
                                                linewidth = LS(1.5)),
              axis.ticks.length = ggplot2::unit(3, "pt"),
              axis.ticks = element_line(size = LS(1.5)),
              axis.minor.ticks.x.bottom = element_line(size = LS(1.5)),)
  )

  update_geom_defaults("boxplot",
                       list(
                         fill = "#ffffd9",
                         linewidth = LS(1),
                         colour = "#010B13"
                       ))
  update_geom_defaults("point",
                       list(
                         shape = 21,
                         fill = "gray99",
                         color = dark_text,
                         size = 1.8,
                         alpha = 1,
                         stroke = 0.5
                       ))

message(paste("ggtext is used to call plot.title and plot.subtitle enabling simple Markdown and HTML",
      "rendering for ggplot2. See https://wilkelab.org/ggtext/"))

message(paste("If saving as a pdf file use 'device = cairo_pdf' to ensure correct font use"))

}
