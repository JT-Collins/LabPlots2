#' @export
grant_style_small <- function(colPal = c("pal1","pal2", "pal3")) {

  #source("pallettes.R") # pull in colour options -- aparently not needed in a package

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

  # Set ggplot2 options and theme settings
  ggplot2::theme_set(
    hrbrthemes::theme_ipsum(
      grid = FALSE,
      axis = TRUE,
      axis_col = "black",
      ticks = TRUE,
      base_size = 7,
      plot_margin = ggplot2::margin(2, 2, 2, 2),
      plot_title_size = 8,
      subtitle_size = 7,
      subtitle_margin = 2,
      axis_title_size = 7,
      strip_text_size = 7,
      caption_size = 7 ,
      caption_face = "plain",
      plot_title_face = "plain",
      caption_margin = 3
    ) + theme(text = element_text(colour = black_text),
              plot.title = ggtext::element_textbox_simple(color = dark_text, lineheight = 0.9,
                                                    margin = margin(b = 2.4),),
              plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.9,
                                                             margin = margin(b = 1.8)),
              plot.caption = ggtext::element_textbox_simple(lineheight = 0.9,
                                                            colour = 	black_text, size = 6),
              plot.caption.position = "plot",
              plot.title.position = "plot",
              axis.line = element_line(lineend = 'square',
                                       linewidth = LS(1),
                                       colour = black_text),
              # axis.line.x = element_line(linewidth = LS(0.75),
              #                            colour = black_text), # 0.75 pt size
              # axis.line.y = element_line(linewidth = LS(0.75),
              #                            colour = black_text), # 0.75 pt size
              axis.title.x = element_text(margin = margin(t = 2, b = 2)),
              axis.title.x.top = element_text(margin = margin(b = 2)),
              axis.title.y = element_text(angle = 90, margin = margin(r = 2)),
              axis.text.y = element_text(colour = black_text, margin = margin(r = 0.8), size = 6),
              axis.text.x = element_text(colour = black_text, margin = margin(t = 0.8), size = 6),
              axis.text.x.top   = element_text(margin = margin(b = 1)),
              axis.text.y.right = element_text(margin = margin(l = 1)),
              axis.ticks.length = unit(2, "pt"),
              axis.minor.ticks.length = unit(1.5, "pt"),
              axis.ticks = element_line(size = LS(1)),
              axis.minor.ticks.x.bottom = element_line(size = LS(1)),


              #panel.grid.major = element_line(linetype='dashed',linewidth = 0.15, colour = mid_text),
              panel.grid.minor = element_blank(),
              panel.spacing = grid::unit(4, "pt"),
              legend.spacing        = unit(4, "pt"),
              legend.margin         = margin(0, 0, 0, 0, "cm"),
              legend.key.size       = grid::unit(0.5, "lines"),
              legend.box.margin     = margin(0, 0, 0, 0, "cm"),
              legend.box.background = element_blank(),
              legend.box.spacing    = unit(2, "pt")


    )
  )

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
                         size = 0.8,
                         alpha = 1,
                         stroke = 0.2
                       ))

  message(paste("ggtext is used to call plot.title and plot.subtitle enabling simple Markdown and HTML",
                "rendering for ggplot2. See https://wilkelab.org/ggtext/"))

  message(paste("If saving as a pdf file use 'device = cairo_pdf' to ensure correct font use"))
}
