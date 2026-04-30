#' Set a global gold-standard ggplot theme
#'
#' @param plot_type Character. One of "paper", "grant", or "presentation".
#' @param base_family Character. Font family to use (default: Arial).
#'
#'
#' @examples
#' set_gold_theme_global()
#' set_gold_theme_global(plot_type = "grant", base_family = "Segoe UI")
#' set_gold_theme_global(plot_type = "presentation")
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
      legend_spacing_y = 1.5,
      legend_key_height = 3.5,
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
      legend_spacing_y = 1,
      legend_key_height = 3,
      axis_line = 0.30,
      tick_length_mm = 0.6,
      margin = margin(4, 5, 4, 4, "pt")
    ),
    presentation = list(
      base = 13,
      title = 15,
      axis_title = 13,
      axis_text = 12,
      legend_text = 12,
      legend_spacing_y = 3,
      legend_key_height = 5,
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
        face = "plain"
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
      panel.grid = ggplot2::element_blank(),

            # Facet strips
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        size = size_map$axis_title,
        colour = "gray20"
      ),

      # Spacing
      plot.margin = size_map$margin,

      # Legend
      legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = size_map$legend_text),
      legend.title = ggplot2::element_blank(),
      legend.spacing.y = grid::unit(size_map$legend_spacing_y, "mm"),
      legend.key.height = grid::unit(size_map$legend_key_height, "mm")

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
      stroke = (size_map$axis_line)*0.8
    )
  )

  message(sprintf(
    "Lab theme set (%s): base_size=%s, font=%s",
    plot_type, size_map$base, base_family
  ))

  invisible(ggplot2::theme_get())
}
