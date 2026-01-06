
# R/zzz.R

.onAttach <- function(libname, pkgname) {
  # Compose a concise, helpful startup message
  msg_lines <- c(
    sprintf("✔ %s loaded: gold-standard plotting utilities", pkgname),
    "   • Use set_gold_theme_global() to apply the journal-ready theme across your session.",
    "   • Save figures with ggsave_gold(..., size = 'half'|'full') for columns, Cairo PDF/EPS, and 300-dpi raster.",
    "   • Palettes available: pal1, pal2, pal3 (see ?scale_color_pal / ?scale_fill_pal)."
  )

  # Optional font check (if systemfonts is available)
  font_line <- NULL
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    fam <- "ArialNarrow"
    ok  <- !is.null(systemfonts::match_font(fam))
    font_line <- if (ok) {
      sprintf("   • Detected font: '%s' ✓ — theme text & geom labels will use it if requested.", fam)
    } else {
      sprintf("   • Font not detected: '%s' — falling back to 'Arial' unless you register it (see ?systemfonts::register_font).", fam)
    }
  } else {
    font_line <- "   • Tip: install {systemfonts} to validate and register fonts cross-platform."
  }

  # Optional auto-enable via env var or option
  auto_line <- NULL
  auto <- getOption("labplots2.auto_theme", Sys.getenv("LABPLOTS2_AUTO_THEME", "FALSE"))
  auto <- isTRUE(as.logical(auto))  # robust coercion
  if (auto) {
    # Safely apply the global theme (no ggplot2 internals, guarded font)
    try({
      set_gold_theme_global(colPal = "pal1", base_family = "ArialNarrow", guides = FALSE)
    }, silent = TRUE)
    auto_line <- "   • Auto-theme: enabled (options('labplots2.auto_theme') / LABPLOTS2_AUTO_THEME)."
  } else {
    auto_line <- "   • To auto-enable each session: options(labplots2.auto_theme = TRUE) or set LABPLOTS2_AUTO_THEME=TRUE."
  }

  # Print the startup message
  packageStartupMessage(paste(c(msg_lines, font_line, auto_line), collapse = "\n"))
}
