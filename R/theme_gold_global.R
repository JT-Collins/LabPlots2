
#' Set the gold-standard theme globally for the current session
#'
#' Applies `theme_gold_standard()` globally via `ggplot2::theme_set()` and
#' updates geom defaults so geoms (e.g., `geom_text()`, `geom_label()`,
#' `ggrepel` labels) use the same `base_family`. Also updates common line
#' weights to device-independent millimeters.
#'
#' @param colPal One of "pal1","pal2","pal3" (must exist in your package/env).
#' @param base_family One of "Arial", "Arial Narrow". Defaults to "Arial Narrow".
#' @param guides Logical; if TRUE, adds subtle light-gray grids to panels.
#' @param check_font Logical; if TRUE (default), validates that `base_family`
#'   is discoverable by {systemfonts} and falls back to "Arial" with a warning
#'   if it is not found.
#' @return Invisibly returns the active theme (as from `ggplot2::theme_get()`).
#' @examples
#' set_gold_theme_global(colPal = "pal1", base_family = "Arial Narrow", guides = FALSE)
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, label = cyl)) +
#'   ggplot2::geom_point() + ggplot2::geom_text()
#' @export
set_gold_theme_global <- function(
    colPal = c("pal1","pal2","pal3"),
    base_family = c("Arial Narrow", "Arial"),
    guides = FALSE,
    check_font = TRUE
) {
  # Validate args
  colPal      <- match.arg(colPal)
  base_family <- match.arg(base_family)

  # Optional: ensure font is discoverable (systemfonts is modern & cross-platform)
  if (check_font) {
    if (!requireNamespace("systemfonts", quietly = TRUE)) {
      warning("Package 'systemfonts' not available; skipping font validation.")
    } else {
      mf <- systemfonts::match_font(base_family)
      if (is.null(mf)) {
        warning(sprintf(
          "Font '%s' not found by systemfonts; falling back to 'Arial'.\n",
          base_family
        ))
        base_family <- "Arial"
      }
    }
  }

  # Build the theme object (pure ggplot2 API; no internals)
  th <- theme_gold_standard(colPal = colPal, base_family = base_family, guides = guides)

  # Set globally
  ggplot2::theme_set(th)

  # Update geom defaults so geoms use the same family (themes do not affect geom text)
  ggplot2::update_geom_defaults("text",  list(family = base_family))
  ggplot2::update_geom_defaults("label", list(family = base_family))

  # If you use ggrepel, align its defaults too
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggplot2::update_geom_defaults("text_repel",  list(family = base_family))
    ggplot2::update_geom_defaults("label_repel", list(family = base_family))
  }

  # Line/point defaults to match gold standards
  axis_line_mm <- 0.30  # ~0.85 pt
  axis_tick_mm <- 0.25  # ~0.7 pt

  ggplot2::update_geom_defaults("line",
                                list(linewidth = axis_line_mm, colour = dark_text))
  ggplot2::update_geom_defaults("errorbar",
                                list(linewidth = axis_line_mm, colour = dark_text))
  ggplot2::update_geom_defaults("boxplot",
                                list(fill = "white", linewidth = axis_line_mm, colour = light_text))
  ggplot2::update_geom_defaults("point",
                                list(shape = 21, fill = "white", colour = dark_text, size = 1.6, stroke = axis_tick_mm))

  message(sprintf("Global theme set: palette=%s, base_family=%s, guides=%s",
                  colPal, base_family, guides))
  invisible(ggplot2::theme_get())
}
