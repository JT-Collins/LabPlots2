#' Save figures at journal-standard sizes while preserving line thickness
#'
#' - Vector: PDF/EPS via Cairo (font embedding, crisp lines), SVG via svglite
#' - Raster: PNG/TIFF at 300 dpi (default), Cairo rendering for antialiasing
#'
#' @param filename Output filename (.pdf, .eps, .svg, .png, .tiff/.tif)
#' @param plot A ggplot object; defaults to last plot
#' @param size "half" (3.5 in width) or "full" (7.0 in width)
#' @param height Height in inches; defaults 2.5 (half) / 4.5 (full)
#' @param dpi Raster resolution; default 300
#' @param bg Background color; default "white"
#' @param family Font family for Cairo vector devices; default "Arial"
#' @export



ggsave_gold <- function(
    filename,
    plot = ggplot2::last_plot(),
    size = c("half", "full"),
    height = NULL,
    dpi = 300,
    bg = "white",
    family = "Arial"
) {
  size <- match.arg(size)

  # Dimensions
  width_in  <- if (size == "half") 3.5 else 7
  height_in <- if (is.null(height)) {
    if (size == "half") 2.5 else 4.5
  } else height

  # Determine extension & type
  ext <- tolower(tools::file_ext(filename))
  is_vector <- ext %in% c("pdf", "eps", "svg")
  is_raster <- ext %in% c("png", "tiff", "tif")

  if (!is_vector && !is_raster) {
    stop("Unsupported file extension '", ext, "'. Use .pdf, .eps, .svg, .png, .tiff/.tif")
  }

  if (is_vector) {
    # ---------------- Vector outputs ----------------
    if (ext == "svg") {
      if (!requireNamespace("svglite", quietly = TRUE)) {
        stop("SVG requested but package 'svglite' is not installed.")
      }
      args <- list(
        filename = filename,
        plot     = plot,
        device   = function(file, width, height, ...) {
          dots <- list(...)
          svglite::svglite(
            file   = file,
            width  = width,
            height = height,
            bg     = if (!is.null(dots$bg)) dots$bg else "white"
          )
        },
        width    = width_in,
        height   = height_in,
        units    = "in",
        bg       = bg,
        scale    = 1
      )
      do.call(ggplot2::ggsave, args)

    } else {
      # PDF/EPS via Cairo to better respect system fonts
      dev_fun <- if (ext == "pdf") grDevices::cairo_pdf else grDevices::cairo_ps
      args <- list(
        filename = filename,
        plot     = plot,
        device   = function(file, width, height, ...) {
          dev_fun(
            file    = file,
            width   = width,
            height  = height,
            onefile = TRUE,
            family  = family
          )
        },
        width    = width_in,
        height   = height_in,
        units    = "in",
        bg       = bg,
        scale    = 1
      )
      do.call(ggplot2::ggsave, args)
    }

  } else {
    # ---------------- Raster outputs (ragg) ----------------
    # Use ragg devices directly; do NOT pass `type` or `compression`
    dev_fun <- if (ext == "png") ragg::agg_png else ragg::agg_tiff

    args <- list(
      filename = filename,
      plot     = plot,
      device   = dev_fun,
      width    = width_in,
      height   = height_in,
      units    = "in",
      dpi      = dpi,
      bg       = bg,
      scale    = 1
    )

    # Note: ragg devices accept 'res' instead of 'dpi' when called directly,
    # but ggplot2::ggsave maps 'dpi' appropriately to raster devices.

    do.call(ggplot2::ggsave, args)
  }

  invisible(filename)
}

