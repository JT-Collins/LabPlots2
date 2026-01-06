
#' Set the gold-standard theme globally for the current session (hardened)
#' @export
set_gold_theme_global <- function(
    colPal = c("pal1","pal2","pal3"),
    base_family = c("Arial Narrow", "Arial"),
    guides = FALSE,
    check_font = TRUE
) {
  colPal      <- match.arg(colPal)
  base_family <- match.arg(base_family)

  # Optional font validation with systemfonts
  if (check_font && requireNamespace("systemfonts", quietly = TRUE)) {
    if (is.null(systemfonts::match_font(base_family))) {
      warning(sprintf(
        "Font '%s' not found by systemfonts; falling back to 'Arial'.",
        base_family
      ))
      base_family <- "Arial"
    }
  }

  # Apply the theme globally (no ggplot2 internals)
  th <- theme_gold_standard(colPal = colPal, base_family = base_family, guides = guides)
  ggplot2::theme_set(th)

  # Geoms don't inherit theme(text=...); update their defaults explicitly
  ggplot2::update_geom_defaults("text",  list(family = base_family))
  ggplot2::update_geom_defaults("label", list(family = base_family))

  # Try to align ggrepel geoms, but only if they are registered
  # (works whether ggrepel is installed, loaded, or not available).
  # Helper: detect available geom names safely
  available_geoms <- tryCatch(
    get("ggplot_global", envir = asNamespace("ggplot2"))$geom_names,
    error = function(e) character(0)
  )

  if ("text_repel"  %in% available_geoms) {
    try(ggplot2::update_geom_defaults("text_repel",  list(family = base_family)), silent = TRUE)
  }
  if ("label_repel" %in% available_geoms) {
    try(ggplot2::update_geom_defaults("label_repel", list(family = base_family)), silent = TRUE)
  }

  # Gold-standard line/point defaults
  axis_line_mm <- 0.30  # ~0.85 pt
  axis_tick_mm <- 0.2  # ~0.7 pt
  ggplot2::update_geom_defaults("line",
                                list(linewidth = axis_line_mm, colour = dark_text))
  ggplot2::update_geom_defaults("errorbar",
                                list(linewidth = axis_line_mm, colour = dark_text))
  ggplot2::update_geom_defaults("boxplot",
                                list(fill = "white", linewidth = axis_line_mm, colour = light_text))
  ggplot2::update_geom_defaults("point",
                                list(shape = 21, fill = "white", colour = dark_text, size = 1.6, stroke = axis_tick_mm))

  message(sprintf(
    "Global theme set: palette=%s, base_family=%s, guides=%s",
    colPal, base_family, guides
  ))

  invisible(ggplot2::theme_get())
}
