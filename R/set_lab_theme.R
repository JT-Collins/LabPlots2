#' Set a global gold-standard ggplot theme
#'
#' @param plot_type Character. One of "paper", "grant", or "presentation".
#' @param base_family Character. Font family to use (default: Arial).
#' @param local Logical. If TRUE, returns the theme object without setting globally.
#'
#' @export
set_lab_theme <- function(
    plot_type = c("paper", "grant", "presentation"),
    base_family = "Arial",
    local = FALSE
) {

  plot_type <- match.arg(plot_type)

  ## -----------------------------------------------------------
  ## Base sizes by use case
  ## -----------------------------------------------------------
  size_map <- switch(
    plot_type,
    paper = list(
      base = 8,
      title = 10,
      axis_title = 8,
      axis_text = 7.5,
      legend_text = 7.5,
      panel_tag = 12,
      legend_spacing_y = 1.5,
      legend_key_height = 3.5,
      axis_line = 0.35,
      tick_length_mm = 0.8,
      margin = ggplot2::margin(5, 6, 5, 5, "pt"),
      title_margin = ggplot2::margin(b = 4, unit = "pt"),
      axis_title_x_margin = ggplot2::margin(t = 4, unit = "pt"),
      axis_title_y_margin = ggplot2::margin(r = 4, unit = "pt"),
      legend_pos = "right",
      legend_dir = "vertical"
    ),
    grant = list(
      base = 7,
      title = 8,
      axis_title = 7,
      axis_text = 7,
      legend_text = 6.5,
      panel_tag = 10,
      legend_spacing_y = 0.5,
      legend_key_height = 2.5,
      axis_line = 0.25,
      tick_length_mm = 0.4,
      margin = ggplot2::margin(2, 2, 2, 2, "pt"),
      title_margin = ggplot2::margin(b = 2, unit = "pt"),
      axis_title_x_margin = ggplot2::margin(t = 2, unit = "pt"),
      axis_title_y_margin = ggplot2::margin(r = 2, unit = "pt"),
      legend_pos = "bottom",   # Horizontal legend at bottom saves width/height footprint
      legend_dir = "horizontal"
    ),
    presentation = list(
      base = 15,
      title = 18,
      axis_title = 15,
      axis_text = 13,
      legend_text = 13,
      panel_tag = 18,
      legend_spacing_y = 4,
      legend_key_height = 8,
      axis_line = 0.7,
      tick_length_mm = 2,
      margin = ggplot2::margin(10, 12, 10, 10, "pt"),
      title_margin = ggplot2::margin(b = 5, unit = "pt"),
      axis_title_x_margin = ggplot2::margin(t = 5, unit = "pt"),
      axis_title_y_margin = ggplot2::margin(r = 5, unit = "pt"),
      legend_pos = "right",
      legend_dir = "vertical"
    )
  )

  text_color <- if (plot_type == "presentation") "grey15" else "gray20"
  line_color <- if (plot_type == "presentation") "grey25" else "gray30"

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
        face =  "plain",
        colour = text_color,
        margin = size_map$title_margin
      ),
      axis.title = ggplot2::element_text(
        size = size_map$axis_title,
        colour = text_color
      ),
      axis.text  = ggplot2::element_text(
        size = size_map$axis_text,
        colour = text_color
      ),

      # Axes
      axis.line  = ggplot2::element_line(
        colour = line_color,
        linewidth = size_map$axis_line
      ),
      axis.ticks = ggplot2::element_line(
        colour = line_color,
        linewidth = size_map$axis_line
      ),
      axis.ticks.length = grid::unit(
        size_map$tick_length_mm,
        "mm"
      ),

      # Panels & Grids
      panel.border = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = if (plot_type == "presentation") {
        ggplot2::element_line(colour = "gray92", linewidth = 0.4)
      } else {
        ggplot2::element_blank()
      },
      panel.grid.minor.y = if (plot_type == "presentation") {
        ggplot2::element_line(colour = "gray92", linewidth = 0.3)
      } else {
        ggplot2::element_blank()
      },

      # Facet strips
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        size = size_map$axis_title,
        face = "plain",
        colour = text_color
      ),

      # Tags
      plot.tag = ggplot2::element_text(
        family = base_family,
        size = size_map$panel_tag,
        face = "bold",
        colour = "black"
      ),
      plot.tag.position = c(0, 1),

      # Spacing
      plot.margin = size_map$margin,
      axis.title.x = ggplot2::element_text(
        margin = size_map$axis_title_x_margin
      ),
      axis.title.y = ggplot2::element_text(
        margin = size_map$axis_title_y_margin
      ),

      # Legend
      legend.key = ggplot2::element_blank(),
      legend.position = size_map$legend_pos,
      legend.direction = size_map$legend_dir,
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(0, 0, 0, 0),
      legend.text = ggplot2::element_text(
        size = size_map$legend_text,
        colour = text_color
      ),
      legend.title = ggplot2::element_text(
        size = size_map$legend_text,
        face = "bold",
        colour = text_color
      ),
      legend.spacing.y = grid::unit(size_map$legend_spacing_y, "mm"),
      legend.key.height = grid::unit(size_map$legend_key_height, "mm")
    )

  ## -----------------------------------------------------------
  ## EARLY EXIT for local use
  ## -----------------------------------------------------------
  if (local) {
    return(th)
  }

  ## -----------------------------------------------------------
  ## Apply globally
  ## -----------------------------------------------------------
  ggplot2::theme_set(th)

  ## -----------------------------------------------------------
  ## Minimal, safe geom defaults
  ## -----------------------------------------------------------
  geom_line_color <- if (plot_type == "presentation") "grey10" else "gray20"

  ggplot2::update_geom_defaults(
    "line",
    list(
      linewidth = if (plot_type == "presentation") 1.0 else size_map$axis_line,
      colour = geom_line_color,
      lineend = "round"
    )
  )
  ggplot2::update_geom_defaults(
    "errorbar",
    list(
      linewidth = if (plot_type == "presentation") 0.8 else size_map$axis_line,
      colour = geom_line_color
    )
  )
  ggplot2::update_geom_defaults(
    "point",
    list(
      shape = 21,
      fill = "white",
      colour = geom_line_color,
      size = switch(plot_type, presentation = 3, grant = 1, paper = 1.2),
      stroke = if (plot_type == "presentation") 1.0 else (size_map$axis_line * 0.8)
    )
  )
  ggplot2::update_geom_defaults(
    "text",
    list(
      family = base_family,
      size = switch(plot_type, presentation = 5.5, grant = 2.4, paper = 2.8),
      colour = text_color
    )
  )

  message(sprintf(
    "Lab theme set (%s): base_size=%s, font=%s",
    plot_type, size_map$base, base_family
  ))

  invisible(ggplot2::theme_get())
}
