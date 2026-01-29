
# R/utils-panel.R

#' image read as ggplot graph
#'
#' @param path the path list with graph name
#' @export

image_read_JuJu <- function(path,
                            plot.margin = margin(t = 5,
                                                 b = 5,
                                                 r = 5,
                                                 l = 5,
                                                 unit = "pt"),
                            panel.background = element_blank(),
                            plot.background = element_blank())
  {

  on.exit(gc())

  if (is.null(names(path))) {

    names(path) <- paste("image",c(1:length(path)),sep = "_")

  }

  img_ls <- JuJu:::.create_results_list(names(path))
  for (i in 1:length(path)) {

    sub_address <- path[i]
    img_na <- names(sub_address)

    img <- image_read(sub_address) %>%
      rasterGrob()

    img <- ggplot() +
      annotation_custom(img,
                        xmin = 0,
                        xmax = 1,
                        ymin = 0,
                        ymax = 1) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      theme(plot.margin = plot.margin,
            plot.background = plot.background,
            panel.background = panel.background,
            aspect.ratio = 1)

    img_ls[img_na] <- list(img)

  }

  return(img_ls)

}

#' image ggplot JuJu
#'
#' @param magick_image the magick image
#' @export

image_ggplot_JuJu <- function(magick_image,
                              plot.margin = margin(t = 5,
                                                   b = 5,
                                                   r = 5,
                                                   l = 5,
                                                   unit = "pt"),
                              panel.background = element_blank(),
                              plot.background = element_blank(),
                              aspect.ratio = NULL)
  {

  on.exit(gc)

  img <- ggplot() +
    annotation_custom(rasterGrob(magick_image),
                      xmin = 0,
                      xmax = 1,
                      ymin = 0,
                      ymax = 1) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme(plot.margin = plot.margin,
          plot.background = plot.background,
          panel.background = panel.background,
          aspect.ratio = aspect.ratio)

  return(img)

}

#' annotate text JuJu
#'
#' @param label the label
#' @param angle the label angle
#' @param size the label size
#' @param family the label family
#' @param face the label face
#' @export

annotate_text_JuJu <- function(label,
                               angle = 0,
                               size = 12,
                               family = "Arial",
                               face = "plain",
                               panel.background = element_blank(),
                               plot.background = element_blank(),
                               vjust = 0.5,
                               hjust = 0.5,
                               plot.margin = margin(0,0,0,0),
                               ratio = NULL){

  on.exit(gc())

  if (is.null(ratio)) {


    if (angle == 0) {

      ratio <- 0.2

    }

    if (angle == 90) {

      ratio <- 5

    }

    if (label == "") {

      ratio <- 1

    }

  }

  p <- ggplot() +
    annotate("text",
             x = 0.5,
             y = 0.5,
             label = label,
             hjust = hjust,
             vjust = vjust,
             size = size,
             angle = angle,
             fontface = face,
             family = family) +
    scale_x_continuous(limits = c(0,1),
                       expand = c(0,0)) +
    scale_y_continuous(limits = c(0,1),
                       expand = c(0,0))+
    theme_void() +
    theme(plot.margin = plot.margin,
          panel.background = panel.background,
          plot.background = plot.background,
          aspect.ratio = ratio)

  return(p)

}

#' combine figures JuJu
#'
#' @param figure_ls the figure list
#' @param col_na the column names
#' @param row_na the row names

combine_figures_JuJu <- function(figure_ls,
                                 col_na,
                                 row_na,
                                 label.size = 12,
                                 label.face = "plain",
                                 plot.margin = margin(t = 5,
                                                      b = 5,
                                                      r = 5,
                                                      l = 5,
                                                      unit = "pt"),
                                 plot.background = element_blank())
  {

  on.exit(gc())

  col_na_ls <- JuJu:::.create_results_list(col_na)
  for (i in 1:length(col_na)) {

    col_na_ls[col_na[i]] <- list(JuJu:::annotate_text_JuJu(label = col_na[i],
                                                           angle = 0,
                                                           size = label.size,
                                                           face = label.face))

  }

  row_na_ls <- JuJu:::.create_results_list(row_na)
  for (i in 1:length(row_na)) {

    row_na_ls[row_na[i]] <- list(JuJu:::annotate_text_JuJu(label = row_na[i],
                                                           angle = 0,
                                                           size = label.size,
                                                           face = label.face))

  }

  white_square <- image_blank(width = 100,
                              height = 100,
                              color = "white") %>%
    rasterGrob()

  blank_img <- ggplot() +
    annotation_custom(white_square,
                      xmin = 0,
                      xmax = 1,
                      ymin = 0,
                      ymax = 1) +
    coord_fixed(ratio = 1,
                xlim = c(0,1),
                ylim = c(0,1),
                expand = FALSE) +
    theme(plot.margin = plot.margin,
          plot.background = element_blank(),
          panel.background = element_blank(),
          aspect.ratio = 1)

  n_rows <- length(figure_ls)
  names(figure_ls) <- row_na

  n_cols <- numeric()
  for (i in 1:n_rows) {

    sub_row <- figure_ls[[i]]

    n_cols <- max(n_cols,length(sub_row))

  }

  comb_row_ls <- JuJu:::.create_results_list(row_na)
  for (i in 1:length(row_na)) {

    sub_figure_ls <- figure_ls[[row_na[i]]]
    if (length(sub_figure_ls) < n_cols) {

      n_sub_cols <- length(sub_figure_ls)

      blank_figure_ls <- lapply(list(1:(n_cols-n_sub_cols)),
                                function(x,blank_img){return(blank_img)},
                                blank_img = blank_img)

      sub_figure_ls <- c(sub_figure_ls,blank_figure_ls)

    }

    sub_comb_row <- wrap_plots(c(row_na_ls[row_na[i]],sub_figure_ls)) +
      plot_layout(nrow = 1)

    comb_row_ls[row_na[i]] <- list(sub_comb_row)

  }

  blank_label <- JuJu::annotate_text_JuJu(label = "",
                                          angle = 90,
                                          size = label.size,
                                          face = label.face)

  col_label <- wrap_plots(c(list(blank_label),col_na_ls)) +
    plot_layout(nrow = 1)

  comb_plot <- wrap_plots(c(comb_row_ls,list(col_label))) +
    plot_layout(nrow = n_rows+1,
                heights = c(rep(1,times = n_rows),0.2)) +
    plot_annotation(theme = theme(plot.background = plot.background,
                                  plot.margin = plot.margin))

  return(comb_plot)

}
