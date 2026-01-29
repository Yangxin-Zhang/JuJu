
# R/utils-graph.R

#' custom ggplot theme
#'
#' @export

theme_JuJu <- function()
  {

  text_title <- element_text(family = "Arial",
                             size = unit(10,
                                         units = "pt"),
                             hjust = 0.5,
                             vjust = 1,
                             margin = margin(unit = "pt",
                                             t = 5,
                                             b = 5,
                                             l = 5,
                                             r = 5))

  custom_theme <- theme(axis.title.x = text_title,
                        axis.title.y = text_title,
                        axis.text.x = element_text(family = "Arial",
                                                   size = unit(8,
                                                               units = "pt"),
                                                   hjust = 1,
                                                   angle = 25),
                        axis.text.y = element_text(family = "Arial",
                                                   size = unit(8,
                                                               units = "pt"),
                                                   hjust = 0),
                        panel.background = element_rect(fill = "white"),
                        panel.grid = element_blank(),
                        legend.title = text_title,
                        plot.margin = margin(unit = "pt",
                                             t = 12,
                                             b = 12,
                                             r = 12,
                                             l = 12),
                        strip.background = element_rect(fill = "white",
                                                        color = "black"),
                        strip.text = text_title,
                        legend.key.size = unit(12,
                                               units = "pt"),
                        legend.text = element_text(family = "Arial",
                                                   size = unit(10,
                                                               units = "pt"),
                                                   hjust = 0),
                        legend.key = element_rect(color = NA),
                        panel.spacing.x = unit(16,
                                               units = "pt"),
                        strip.background.x = element_rect(colour = "white",
                                                          fill = "white"),
                        axis.line = element_line(colour = "black"))

  return(custom_theme)

}

#' save ggplot2 graph
#'
#' @param plot the plot to save
#' @export

ggsave_JuJu <- function(plot) {

  on.exit(gc())

  save_path <- getwd()
  ggsave(plot = plot,
         device = "png",
         dpi = 600,
         height = 210,
         width = 297,
         units = "mm",
         filename = "test",
         path = save_path)

}
