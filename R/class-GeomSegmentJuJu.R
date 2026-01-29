
# R/class-GeomSegmentJuJu.R

#' GeomSegmentJuJu
#'
#' @exportClass GeomSegmentJuJu

GeomSegmentJuJu <-ggproto("GeomSegmentJuJu",
                          GeomSegment,
                          required_aes = c("x",
                                           "y",
                                           "main.group"),
                          draw_panel = function (self,
                                                 data,
                                                 panel_params,
                                                 coord,
                                                 width,
                                                 arrow,
                                                 arrow.fill,
                                                 lineend,
                                                 linejoin,
                                                 na.rm,
                                                 linewidth,
                                                 flipped_aes = FALSE){

                            cat("successful",width,"\n")
                            grid::gList(GeomSegment$draw_panel(JuJu:::.transform_data_JuJu(data = data,
                                                                                                  geom_symbol = "segment"),
                                                               panel_params,
                                                               coord))

                          })

#' geom_segment_JuJu
#'
#' @export

geom_segment_JuJu <- function (mapping = NULL,
                               data = NULL,
                               stat = "identity",
                               position = position_dodge(width = 0.6),
                               arrow = NULL,
                               arrow.fill = NULL,
                               lineend = "butt",
                               linejoin = "round",
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               width = NULL,
                               ...)
{
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomSegmentJuJu,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(arrow = arrow,
                       arrow.fill = arrow.fill,
                       lineend = lineend,
                       linejoin = linejoin,
                       na.rm = na.rm,
                       width = width,
                       ...))
}
