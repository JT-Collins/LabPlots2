
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
  width_in <- if (size == "half") 3.5 else 7.0
  height_in <- if (is.null(height)) if (size == "half") 2.5 else 4.5 else height

  ext <- tolower(tools::file_ext(filename))
  is_vector <- ext %in% c("pdf", "eps", "svg")
  is_raster <- ext %in% c("png", "tiff", "tif")

  if (!is_vector && !is_raster) {
    stop("Unsupported file extension '", ext,
         "'. Use .pdf, .eps, .svg, .png, .tiff/.tif")
  }

  if (is_vector) {
    # Choose device function
    if (ext == "svg") {
      if (!requireNamespace("svglite", quietly = TRUE)) {
        stop("SVG requested but package 'svglite' is not installed.")
      }
      # svglite accepts bg; pass it through
      dev_fun <- function(file, width, height, bg, family) {
        svglite::svglite(file = file, width = width, height = height, bg = bg)
      }
      ggplot2::ggsave(
        filename = filename,
        plot = plot,
        device = function(file, width, height, ...) {
          dots <- list(...)
          dev_fun(file = file, width = width, height = height,
                  bg = dots[["bg"]] %||% "white", family = family)
        },
        width = width_in, height = height_in, units = "in",
        bg = bg, scale = 1
      )

    } else {
      # PDF/EPS via Cairo: accept only supported args (no bg/useDingbats)
      dev_fun <- if (ext == "pdf") grDevices::cairo_pdf else grDevices::cairo_ps

      ggplot2::ggsave(
        filename = filename,
        plot = plot,
        device = function(file, width, height, ...) {
          # Strip unsupported args that ggsave passes (bg, dpi, useDingbats, etc.)
          dev_fun(file = file, width = width, height = height,
                  onefile = TRUE, family = family)
        },
        width = width_in, height = height_in, units = "in",
        # 'bg' is handled by plot background; Cairo devices ignore it
        bg = bg, scale = 1
      )
    }

  } else {
    # Raster devices accept bg, dpi, type, compression
    dev_name <- if (ext == "png") "png" else "tiff"
    ggplot2::ggsave(
      filename = filename,
      plot = plot,
      device = dev_name,
      width = width_in, height = height_in, units = "in",
      dpi = dpi, bg = bg, type = "cairo",
      compression = if (ext %in% c("tiff","tif")) "lzw" else "none",
      scale = 1
    )
  }

  invisible(filename)
}
