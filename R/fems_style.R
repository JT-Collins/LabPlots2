#' @export
#
# Style based on image requirements for FEMS
fems_style <- function(colPal = c("pal1","pal2", "pal3")) {

  # Check if required packages are installed and load the packages
  required_packages <- c("hrbrthemes", "ggplot2", "grid", "ggtext")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    install_commands <- paste("install.packages('", missing_packages, "')", sep = "")
    stop(paste("The following packages are required:", paste(missing_packages, collapse = ", "),
               "please install using", paste(install_commands, collapse = " or ")))
  }

  colPal <- match.arg(colPal)

  options(ggplot2.continuous.colour="viridis")
  options(ggplot2.continuous.fill = "viridis")

  if (colPal == "pal2"){
    options(ggplot2.discrete.colour = pal2 )
    options(ggplot2.discrete.fill = pal2 )
  } else if (colPal == "pal3"){
    options(ggplot2.discrete.colour = pal3 )
    options(ggplot2.discrete.fill = pal3 )

  } else {
    options(ggplot2.discrete.colour = pal1 )
    options(ggplot2.discrete.fill = pal1 )

  }

  # Set ggplot2 options and theme settings
  ggplot2::theme_set(
    hrbrthemes::theme_ipsum(
      grid = FALSE,
      axis = TRUE,
      axis_col = "#4a5763",
      ticks = FALSE,
      base_size = 10,
      plot_margin = ggplot2::margin(2, 2, 2, 2),
      plot_title_size = 10,
      subtitle_size = 9,
      subtitle_margin = 4,
      axis_title_size = 10,
      strip_text_size = 10,
      caption_size = 9,
      caption_face = "plain",
      caption_margin = 4
    ) + theme(text = element_text(colour = black_text),
              plot.title = ggtext::element_markdown(color = dark_text, lineheight = 1.1,
                                                    margin = margin(b = 2.4),),
              plot.subtitle = ggtext::element_textbox_simple(lineheight = 1.1,
                                                             margin = margin(b = 1.8)),
              plot.caption = ggtext::element_textbox_simple(lineheight = 1.1, colour = 	"#010B13"),
              plot.caption.position = "panel",
              plot.title.position = "panel",
              axis.line = element_line(lineend = 'square'),
              axis.line.x = element_line(linewidth = LS(1.2), colour = '#4a5763'), # 1.2 is pt size
              axis.line.y = element_line(linewidth = LS(1.2), colour = '#4a5763'), # 1.2 is pt size
              axis.title.x = element_text(#hjust = 0.5,
                                          margin = margin(t = 3, b = 3)),
              axis.title.x.top = element_text(margin = margin(b = 3)),
              axis.title.y = element_text(angle = 90,
                                          #hjust = 0.5,
                                          margin = margin(r = 3)),
              axis.text.y = element_text(size = 9,
                                         colour = black_text,
                                         margin = margin(r = 0.8)),
              axis.text.x = element_text(size = 9,
                                         colour = black_text,
                                         margin = margin(t = 0.8)),
              axis.text.x.top   = element_text(margin = margin(b = 0.8)),
              axis.text.y.right = element_text(margin = margin(l = 0.8)),
              axis.ticks.length = unit(2.5, "pt"),
              axis.ticks = element_line(linewidth = LS(1.2), colour = '#4a5763'),
              # panel.grid.major = element_line(linetype='dashed'),
              # panel.grid.minor = element_blank(),
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

