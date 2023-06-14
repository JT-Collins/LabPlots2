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
  uofl_col <- c(
    "#ad0000",
    "#004e74",
    "#8b9da1",
    "#00a89d",
    "#ffba00",
    "#544009",
    "#698900",
    "#ddcb2e"
  )

  alt_col <- c(
    '#444444',
    '#de6757',
    '#466eb4',
    '#d7642c',
    '#af4b91',
    '#00a0e1',
    '#41a65c',
    '#5E2C25',
    '#78695F'
  )


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

  # Set ggplot2 options and theme settings
  ggplot2::theme_set(
    hrbrthemes::theme_ipsum(
      grid = TRUE,
      axis = FALSE,
      axis_col = "#5A5A5A",
      ticks = FALSE,
      base_size = 8,
      plot_margin = ggplot2::margin(2, 2, 2, 2),
      plot_title_size = 10,
      subtitle_size = 9,
      subtitle_margin = 4,
      axis_title_size = 9,
      strip_text_size = 9,
      caption_size = 8,
      caption_face = "plain",
      caption_margin = 4
          ) + theme(text = element_text(colour = mid_text),
                    plot.title = ggtext::element_markdown(color = dark_text, lineheight = 1.1,
                                                          margin = margin(b = 4.8),),
                    plot.subtitle = ggtext::element_textbox_simple(lineheight = 1.1,
                                                             margin = margin(b = 3.6)),
                    plot.caption = ggtext::element_textbox_simple(lineheight = 1.1, colour = 	"#010B13"),
                    plot.caption.position = "plot",
                    plot.title.position = "plot",
                    axis.title.x = element_text(margin = margin(t = 3, b = 3)),
                    axis.title.x.top = element_text(margin = margin(b = 4)),
                    axis.title.y = element_text(angle = 90, margin = margin(r = 4)),
                    axis.text.y = element_text(colour = light_text, margin = margin(r = 1.6)),
                    axis.text.x = element_text(colour = light_text, margin = margin(t = 1.6)),
                    axis.text.x.top   = element_text(margin = margin(b = 1.6)),
                    axis.text.y.right = element_text(margin = margin(l = 1.6)),
                    axis.ticks.length = unit(2, "pt"),
                    panel.grid.major = element_line(linetype='dashed'),
                    panel.grid.minor = element_blank(),
                    panel.spacing = grid::unit(6, "pt"),
                    legend.spacing        = unit(4, "pt"),
                    legend.margin         = margin(0, 0, 0, 0, "cm"),
                    legend.key.size       = grid::unit(0.5, "lines"),
                    legend.box.margin     = margin(0, 0, 0, 0, "cm"),
                    legend.box.background = element_blank(),
                    legend.box.spacing    = unit(4, "pt")

  ))

  update_geom_defaults("boxplot",
                       list(
                         fill = "gray98",
                         linewidth = 0.2,
                         colour = light_text
                       ))
  update_geom_defaults("point",
                       list(
                         shape = 21,
                         fill = "gray90",
                         color = dark_text,
                         size = 1.8,
                         alpha = 1,
                         stroke = 0.2
                       ))

  message(paste("ggtext is used to call plot.title and plot.subtitle enabling simple Markdown and HTML",
                "rendering for ggplot2. See https://wilkelab.org/ggtext/"))

  message(paste("If saving as a pdf file use 'device = cairo_pdf' to ensure correct font use"))
}
