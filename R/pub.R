
#' Gold-standard theme object (safe, public API, no internals)
#'
#' Returns a ggplot2 theme with white background, journal-style text sizes,
#' and device-independent line widths. No global side effects.
#'
#' @param colPal One of "pal1", "pal2", "pal3" (must exist in calling env).
#' @param base_family One of "Arial", "Arial Narrow".
#' @param guides Logical; if TRUE, adds subtle light-gray grid lines.
#' @return A ggplot2 theme object.
#' @export
theme_gold_standard <- function(
    colPal = c("pal1","pal2","pal3"),
    base_family = c("Arial", "Arial Narrow"),
    guides = FALSE
) {
  # --- Dependencies (runtime) ---
  required_packages <- c("ggplot2", "grid")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    install_commands <- paste("install.packages('", missing_packages, "')", sep = "")
    stop(paste("The following packages are required:", paste(missing_packages, collapse = ", "),
               "please install using", paste(install_commands, collapse = " or ")))
  }

  # --- Palettes (explicit scales recommended; theme does not change options) ---
  colPal <- match.arg(colPal)
  base_family <- match.arg(base_family)

  # Text sizes (pt)
  title_pt       <- 12
  subtitle_pt    <- 10
  axis_title_pt  <- 9
  strip_pt       <- 9
  tick_pt        <- 8
  legend_pt      <- 8
  caption_pt     <- 8
  base_size_pt   <- 8

  # Line widths (mm) and spacing
  axis_line_mm       <- 0.30
  axis_tick_mm       <- 0.25
  tick_len_mm        <- 2.5
  panel_spacing_mm   <- 3
  guide_col          <- "#D9D9D9"
  guide_lw           <- 0.20

  # Build theme (pure ggplot2::theme; no internals)
  th <- ggplot2::theme(
    text = ggplot2::element_text(family = base_family, colour = black_text, size = base_size_pt),

    # Titles & caption
    plot.title    = ggplot2::element_text(color = dark_text, size = title_pt, face = "plain",
                                          margin = ggplot2::margin(b = 6)),
    plot.subtitle = ggplot2::element_text(color = dark_text, size = subtitle_pt,
                                          margin = ggplot2::margin(b = 4)),
    plot.caption  = ggplot2::element_text(color = black_text, size = caption_pt,
                                          margin = ggplot2::margin(t = 6)),

    # Axes
    axis.title.x = ggplot2::element_text(size = axis_title_pt,
                                         margin = ggplot2::margin(t = 6, b = 6)),
    axis.title.y = ggplot2::element_text(size = axis_title_pt, angle = 90,
                                         margin = ggplot2::margin(r = 6)),
    axis.text.x  = ggplot2::element_text(color = black_text, size = tick_pt,
                                         margin = ggplot2::margin(t = 3)),
    axis.text.y  = ggplot2::element_text(color = black_text, size = tick_pt,
                                         margin = ggplot2::margin(r = 3)),
    axis.ticks.length = grid::unit(tick_len_mm, "mm"),
    axis.ticks        = ggplot2::element_line(linewidth = axis_tick_mm, colour = black_text),
    axis.line         = ggplot2::element_line(lineend = "square",
                                              linewidth = axis_line_mm, colour = black_text),

    # Panels
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
    panel.grid.major = if (guides) ggplot2::element_line(linewidth = guide_lw, colour = guide_col) else ggplot2::element_blank(),
    panel.grid.minor = if (guides) ggplot2::element_line(linewidth = guide_lw * 0.75, colour = guide_col) else ggplot2::element_blank(),
    panel.spacing    = grid::unit(panel_spacing_mm, "mm"),

    # Strips & legend
    strip.text        = ggplot2::element_text(size = strip_pt, colour = dark_text),
    legend.text       = ggplot2::element_text(size = legend_pt, colour = black_text),
    legend.title      = ggplot2::element_text(size = legend_pt, colour = dark_text),
    legend.key        = ggplot2::element_rect(fill = "white", colour = NA),
    legend.key.width  = grid::unit(8, "pt"),
    legend.key.height = grid::unit(8, "pt"),
    legend.spacing    = grid::unit(4, "pt"),
    legend.box.margin = ggplot2::margin(0, 0, 0, 0, "cm")
  )

  th
}

#' Convenience: set theme globally and update geom defaults (opt-in)
#'
#' @param colPal One of "pal1","pal2","pal3".
#' @param base_family "Arial" or "Arial Narrow".
#' @param guides Logical; TRUE adds subtle gridlines.
#' @export
use_gold_theme <- function(
    colPal = c("pal1","pal2","pal3"),
    base_family = c("Arial", "Arial Narrow"),
    guides = FALSE
) {
  th <- theme_gold_standard(colPal = colPal, base_family = base_family, guides = guides)
  ggplot2::theme_set(th)

  # Update geom defaults to match line weights & typographic intent (safe)
  axis_line_mm <- 0.30
  axis_tick_mm <- 0.25

  ggplot2::update_geom_defaults("line",
                                list(linewidth = axis_line_mm, colour = dark_text))
  ggplot2::update_geom_defaults("errorbar",
                                list(linewidth = axis_line_mm, colour = dark_text))
  ggplot2::update_geom_defaults("boxplot",
                                list(fill = "white", linewidth = axis_line_mm, colour = light_text))
  ggplot2::update_geom_defaults("point",
                                list(shape = 21, fill = "white", colour = dark_text,
                                     size = 1.6, stroke = axis_tick_mm))

  message("Gold theme set globally. Note: global state changes can affect other plots in the session.")
}

#' Palette scale helpers (avoid using global options)
#' Add these to a plot to apply your discrete palettes explicitly.
#' @export
scale_color_pal <- function(choice = c("pal1", "pal2", "pal3")) {
  choice <- match.arg(choice)
  values <- switch(choice, pal1 = pal1, pal2 = pal2, pal3 = pal3)
  ggplot2::scale_color_manual(values = values)
}

#' @export
scale_fill_pal <- function(choice = c("pal1", "pal2", "pal3")) {
  choice <- match.arg(choice)
  values <- switch(choice, pal1 = pal1, pal2 = pal2, pal3 = pal3)
  ggplot2::scale_fill_manual(values = values)
}

#' Backward-compatible wrapper keeping your function name
#' (calls the safer internals above; no ggplot2 internals are used)
#' @export
grant_style_small <- function(
    colPal = c("pal1","pal2","pal3"),
    base_family = c("Arial", "Arial Narrow"),
    guides = FALSE
) {
  use_gold_theme(colPal = colPal, base_family = base_family, guides = guides)
  message("For PDFs, use device = cairo_pdf. For PNG/TIFF, export at 300 dpi.")
}
