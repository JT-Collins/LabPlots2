#' @export
grant_style <- function() {

  uofl_col <- c("#8B9DA1", "#AD0000", "#004E74", "#FEBE10" ,"#00A89D","#7A6C53",  "#AAB43A", "#D9C982")
  options(ggplot2.continuous.colour="viridis")
  options(ggplot2.continuous.fill = "viridis")
  options(ggplot2.discrete.colour = uofl_col )
  options(ggplot2.discrete.fill = uofl_col )

  hrbrthemes::import_roboto_condensed()
  ggplot2::theme_set(hrbrthemes::theme_ipsum_rc(grid = F,
                                                axis = F,
                                                ticks = T,
                                                base_size = 10,
                                                plot_margin = ggplot2::margin(10, 5, 5, 10),
                                                panel_spacing = grid::unit(1, "lines"),
                                                plot_title_size = 12,
                                                subtitle_size = 11,
                                                axis_title_size = 12,
                                                strip_text_size = 10) +
                       ggplot2::theme(plot.title = ggplot2::element_text(color = "grey30"),
                                      plot.title.position = "plot",
                                      axis.line = element_line(colour = "grey20", size = 0.4),
                                      axis.ticks.length = unit(3, "pt"),
                                      axis.text=element_text(color="grey20")))

}
