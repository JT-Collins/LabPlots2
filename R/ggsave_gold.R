
#' Save figures at gold-standard sizes while preserving line thickness
#' @param filename Output file name (.pdf, .png, .tiff)
#' @param plot A ggplot object; defaults to last plot
#' @param size "half" (3.5 in width) or "full" (7 in width)
#' @param height Height in inches; if NULL, uses sensible defaults
#' @param dpi For raster formats (PNG/TIFF), default 300
#' @param bg Background color; default "white"
#' @param family Font family passed to vector devices (e.g., "Arial")
#' @param useDingbats Logical for PDF/EPS embedding; default FALSE
#' @export
ggsave_gold <- function(
    filename,
    plot = ggplot2::last_plot(),
    size = c("half", "full"),
    height = NULL,
    dpi = 300,
    bg = "white",
    family = "Arial",
    useDingbats = FALSE
) {
  size <- match.arg(size)
  width_in <- if (size == "half") 3.5 else 7.0
  if (is.null(height)) {
    height_in <- if (size == "half") 2.5 else 4.5
  } else {
    height_in <- height
  }

  ext <- tolower(tools::file_ext(filename))
  is_vector <- ext %in% c("pdf", "eps", "svg")
  is_raster <- ext %in% c("png", "tiff", "tif")

  if (is_vector) {
    # Use Cairo to embed fonts reliably
    dev_fun <- switch(ext,
                      pdf = grDevices::cairo_pdf,
                      eps = grDevices::cairo_ps,
                      svg = if (requireNamespace("svglite", quietly = TRUE)) svglite::svglite else NULL
    )
    if (is.null(dev_fun)) {
      stop("For .svg outputs, please install the 'svglite' package.")
    }

    ggplot2::ggsave(
      filename = filename,
      plot = plot,
      device = function(file, width, height) dev_fun(file = file, width = width, height = height,
                                                     family = family, onefile = TRUE, fallback_resolution = 300,
                                                     useDingbats = useDingbats),
      width  = width_in, height = height_in, units = "in",
      bg = bg, scale = 1
    )

  } else if (is_raster) {
    # Raster: preserve white background; 300 dpi; Cairo type for anti-aliased lines
    dev_name <- if (ext %in% c("png")) "png" else "tiff"
    ggplot2::ggsave(
      filename = filename,
      plot = plot,
      device = dev_name,
      width = width_in, height = height_in, units = "in",
      dpi = dpi, bg = bg, type = "cairo", # ensures consistent rendering
      compression = if (ext %in% c("tiff","tif")) "lzw" else "none",
      scale = 1
    )

  } else {
    stop("Unsupported file extension. Use .pdf, .eps, .svg, .png, .tiff/.tif")
  }
}
