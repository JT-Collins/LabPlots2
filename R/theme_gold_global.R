#' Set a global gold-standard ggplot theme
#'
#' @param plot_type Character. One of "paper", "grant", or "presentation".
#' @param base_family Character. Font family to use (default: Arial).
#'
#' @export
set_lab_theme <- function(
    plot_type = c("paper", "grant", "presentation"),
    base_family = "Arial"
) {

  plot_type <- match.arg(plot_type)

  ## -----------------------------------------------------------
  ## Base sizes by use case
  ## -----------------------------------------------------------
  size_map <- switch(
    plot_type,
    paper = list(
      base = 8,
      title = 9,
      axis_title = 8,
      axis_text = 7.5,
      legend_text = 7,
      axis_line = 0.35,
      tick_length_mm = 1,
      margin = margin(5, 6, 5, 5, "pt")
    ),
    grant = list(
      base = 7,
      title = 8,
      axis_title = 7,
      axis_text = 6.5,
      legend_text = 6.5,
      axis_line = 0.30,
      tick_length_mm = 0.8,
      margin = margin(4, 5, 4, 4, "pt")
    ),
    presentation = list(
      base = 12,
      title = 14,
      axis_title = 12,
      axis_text = 11,
      legend_text = 11,
      axis_line = 0.6,
      tick_length_mm = 2,
      margin = margin(10, 12, 10, 10, "pt")
    )
  )

  ## -----------------------------------------------------------
  ## Build theme
  ## -----------------------------------------------------------
  th <- ggplot2::theme_bw(
    base_size = size_map$base,
    base_family = base_family
  ) +
    ggplot2::theme(

      # Text
      plot.title = ggplot2::element_text(
        size = size_map$title,
        face = "plain",
        hjust = 0.5
      ),
      axis.title = ggplot2::element_text(size = size_map$axis_title),
      axis.text  = ggplot2::element_text(
        size = size_map$axis_text,
        colour = "gray20"
      ),

      # Axes
      axis.line  = ggplot2::element_line(
        colour = "gray30",
        linewidth = size_map$axis_line
      ),
      axis.ticks = ggplot2::element_line(
        colour = "gray30",
        linewidth = size_map$axis_line
      ),
      axis.ticks.length = grid::unit(
        size_map$tick_length_mm,
        "mm"
      ),

      # Panels
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),

      # Spacing
      plot.margin = size_map$margin,

      # Legend
      legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = size_map$legend_text),
      legend.title = ggplot2::element_blank()
    )

  ## -----------------------------------------------------------
  ## Apply globally
  ## -----------------------------------------------------------
  ggplot2::theme_set(th)

  ## -----------------------------------------------------------
  ## Minimal, safe geom defaults
  ## -----------------------------------------------------------
  ggplot2::update_geom_defaults(
    "line",
    list(linewidth = size_map$axis_line, colour = "gray20")
  )
  ggplot2::update_geom_defaults(
    "errorbar",
    list(linewidth = size_map$axis_line, colour = "gray20")
  )
  ggplot2::update_geom_defaults(
    "point",
    list(
      shape = 21,
      fill = "white",
      colour = "gray20",
      size = if (plot_type == "presentation") 3 else 1.6,
      stroke = size_map$axis_line
    )
  )

  message(sprintf(
    "Lab theme set (%s): base_size=%s, font=%s",
    plot_type, size_map$base, base_family
  ))

  invisible(ggplot2::theme_get())
}
