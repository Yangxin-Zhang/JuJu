
# R/utils-geom_segment_JuJu.R

#' transform data JuJu
#'
#' @param data the data
#' @param geom_symbol the geom symbol

.transform_data_JuJu <- function(data,
                                 geom_symbol)
  {

  on.exit(gc())

  if (geom_symbol == "segment") {

    data <- as.data.table(data)

    sub_groups <- split(data,by = "main.group")

    if (length(sub_groups) == nrow(data)) {

      sub_groups <- list(data)

    }

    segment_interval <- max(data[,y]*0.1)

    transform_subgroup <- function(sub_group,
                                   segment_interval){

      x_value <- sub_group[,x]
      x_value_pairs <- JuJu:::.create_group_pairs(x_value,
                                                  annotate_interval = TRUE)

      create_new_data <- function(sub_pair,
                                  segment_interval,
                                  sub_group){

        data_new <- data.table(x = c(sub_pair[1],
                                     sub_pair[1],
                                     sub_pair[2]),
                               xend = c(sub_pair[2],
                                        sub_pair[1],
                                        sub_pair[2]),
                               y = c((max(sub_group[,y])+segment_interval*2),
                                     (sub_group[1,y]+segment_interval),
                                     (sub_group[2,y]+segment_interval)),
                               yend = c((max(sub_group[,y])+segment_interval*2),
                                        (max(sub_group[,y])+segment_interval*2),
                                        (max(sub_group[,y])+segment_interval*2)),
                               PANEL = 1,
                               colour = "black",
                               linewidth = 0.5,
                               linetype = 1,
                               alpha = NA)

        return(data_new)

      }

      data_new_ls <- lapply(x_value_pairs,
                            create_new_data,
                            segment_interval = segment_interval,
                            sub_group = sub_group)

      return(rbindlist(data_new_ls))

    }

    data <- rbindlist(lapply(sub_groups,
                             transform_subgroup,
                             segment_interval = segment_interval))

    data[,group := 1]

    return(as.data.frame(data))

  }

  if (geom_symbol == "errorbar") {

    data <- as.data.table(data)

    data[,ymin := y_min]
    data[,ymax := y_max]

    data[,x_max := xmax]
    data[,x_min := xmin]
    data[,xmax := (x_max-(x_max-x_min)/3)]
    data[,xmin := (x_min+(x_max-x_min)/3)]
    data[,width := 0.2]

    return(as.data.frame(data))

  }

}
