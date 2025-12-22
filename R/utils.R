
#### Get required (optional) ggplot2 functions — SAFE LOOKUP -------------------

# Instead of hard-failing on missing internals, we guard lookups and
# provide fallbacks. Do NOT rely on non-public ggplot2 internals.

# Safe resolver for namespace objects; returns NULL if not present
.get_ns_obj <- function(pkg, name) {
  if (!requireNamespace(pkg, quietly = TRUE)) return(NULL)
  ns <- asNamespace(pkg)
  if (exists(name, envir = ns, inherits = FALSE)) {
    get(name, envir = ns, inherits = FALSE)
  } else {
    NULL
  }
}

# Lazily grab a *subset* of potentially useful functions
# NOTE: 'axis_label_element_overrides' REMOVED to avoid load failures.
.grab_ggplot_internals <- function() {
  objects <- c(
    # Prefer public APIs; keep only symbols you truly need and guard them.
    "absoluteGrob",     # sometimes re-exported; fallback to grid::absoluteGrob
    "draw_axis_labels", # internal; may be NULL in modern ggplot2
    "parse_safe"        # internal; we provide a safe fallback below
  )

  available <- setNames(vector("list", length(objects)), objects)
  for (i in objects) {
    available[[i]] <- .get_ns_obj("ggplot2", i)
  }
  available
}

# Do NOT eagerly abort at load time: attempt the grab but tolerate failures.
.ggint <- tryCatch(.grab_ggplot_internals(), error = function(e) list())

# Provide robust fallbacks so usage won't explode later ------------------------

# absoluteGrob fallback (public from 'grid')
if (is.null(.ggint$absoluteGrob)) {
  if (requireNamespace("grid", quietly = TRUE)) {
    .ggint$absoluteGrob <- grid::absoluteGrob
  } else {
    .ggint$absoluteGrob <- function(...) {
      stop("grid::absoluteGrob is unavailable; please install/load 'grid'.")
    }
  }
}

# parse_safe fallback (parse labels defensively)
if (is.null(.ggint$parse_safe)) {
  .ggint$parse_safe <- function(x) {
    # 'x' can be a character vector of labels
    out <- tryCatch(parse(text = x), error = function(e) x)
    out
  }
}

# draw_axis_labels fallback (encourage public guide usage)
if (is.null(.ggint$draw_axis_labels)) {
  .ggint$draw_axis_labels <- function(...) {
    stop(
      "ggplot2 internal 'draw_axis_labels' not available.\n",
      "Use public APIs instead: ggplot2::guide_axis(...) and theme(axis.text=...)."
    )
  }
}

#### Helper functions for stat_pvalue_manual -----------------------------------

# Guess the column to be used as the significance labels
guess_signif_label_column <- function(data) {
  potential.label <- c(
    "label", "labels", "p.adj.signif", "p.adj", "padj",
    "p.signif", "p.value", "pval", "p.val", "p"
  )
  res <- intersect(potential.label, colnames(data))
  if (length(res) > 0) {
    res <- res[1]
  } else {
    stop("label is missing")
  }
  res
}

# Validate p-value x position
validate_x_position <- function(x, data) {
  if (is.numeric(x)) {
    number.of.test <- nrow(data)
    number.of.xcoord <- length(x)
    xtimes <- number.of.test / number.of.xcoord
    if (number.of.xcoord < number.of.test) x <- rep(x, xtimes)
  } else if (is.character(x)) {
    if (!(x %in% colnames(data)))
      stop("can't find the x variable '", x, "' in the data")
  }
  return(x)
}

# Validate p-value y position
validate_y_position <- function(y.position, data) {
  if (is.numeric(y.position)) {
    number.of.test <- nrow(data)
    number.of.ycoord <- length(y.position)
    xtimes <- number.of.test / number.of.ycoord
    if (number.of.ycoord < number.of.test) y.position <- rep(y.position, xtimes)
  } else if (is.character(y.position)) {
    if (!(y.position %in% colnames(data)))
      stop("can't find the y.position variable '", y.position, "' in the data")
  }
  return(y.position)
}

keep_only_tbl_df_classes <- function(x) {
  to.remove <- setdiff(class(x), c("tbl_df", "tbl", "data.frame"))
  if (length(to.remove) > 0) {
    class(x) <- setdiff(class(x), to.remove)
  }
  x
}

# For control rows: the comparison of control against itself
# Used only when positioning the labels for grouped bars
add_ctr_rows <- function(data, ref.group) {
  xmin <- NULL
  data <- keep_only_tbl_df_classes(data)
  ctr <- data[!duplicated(data$xmin), ]
  ctr$group2 <- ref.group
  ctr$label <- " "
  rbind(ctr, data)
}

#### Helper functions for geom_bracket -----------------------------------------

# Add increments to bracket height
add_step_increase <- function(data, step.increase) {
  comparisons.number <- 0:(nrow(data) - 1)
  step.increase <- step.increase * comparisons.number
  data$step.increase <- step.increase
  data
}

# Guess column to be used as significance label
guess_signif_label_column <- function(data) {
  potential.label <- c(
    "label", "labels", "p.adj.signif", "p.adj", "padj",
    "p.signif", "p.value", "pval", "p.val", "p"
  )
  res <- intersect(potential.label, colnames(data))
  if (length(res) > 0) {
    res <- res[1]
  } else {
    stop("label is missing")
  }
  res
}

#' @title Convert font sizes measured as points to ggplot font sizes
#' @description Converts font sizes measured as points (as given by most programs such as MS Word etc.) to ggplot font sizes
#' @param x numeric vector giving the font sizes in points
#' @return numeric vector of ggplot font sizes
#' @keywords internal
#' @export
FS <- function(x) x / 2.845276

#' @title Convert line sizes measured as points to ggplot line sizes
#' @description Converts line sizes measured as points (as given by Adobe Illustrator etc.) to ggplot line sizes
#' @param x numeric vector giving the line sizes in points
#' @return numeric vector of ggplot line sizes
#' @keywords internal
#' @export
LS <- function(x) x / 2.13

#' @title default ggsave for PowerPoint figs
#' @description saves at sensible default for PowerPoint
#' @keywords internal
#' @export
pp.ggsave <- function(filename = default_name(plot), height = 10, width = 5.625, ...) {
  ggplot2::ggsave(filename = filename, height = height, width = width, ...)
}
