# Theme for plots
mytheme <- ggplot2::theme_bw() + ggplot2::theme(axis.text = ggplot2::element_text(size = 16),
                                                axis.title.x = ggplot2::element_text(size = 18),
                                                axis.title.y = ggplot2::element_text(size = 18, angle = 90))

#' @export
plotByDay <- function(perdayCount, useTeamGrouping) {
  plotdata <- perdayCount %>%
    reshape2::dcast(date ~ teamName, value.var = 'n', fill = 0) %>%
    reshape2::melt(., id.vars = c("date"),
                   variable.name = "teamName", value.name = "n") %>%
    dplyr::rename(group = teamName)

  p <- ggplot2::ggplot(plotdata, ggplot2::aes(x = date, y = n))
  p <- p + ggplot2::geom_line(ggplot2::aes(group = group, color = group), size = 1)

  if (useTeamGrouping) {
    p <- p + ggplot2::scale_color_brewer(palette = "Set1")
  } else {
    p <- p + ggplot2::scale_color_manual(values = "black")
  }

  p  <- p + mytheme + ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                                     axis.text.x = ggplot2::element_text(size = 16, angle = 270),
                                     legend.position = "top")
  p

}
