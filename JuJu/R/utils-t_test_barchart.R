
# R/utils-t_test_barchart.R

#' generate additional dataset from plotting dataset
#'
#' @param plotting_dataset the plotting dataset
#' @param test.type the test type
#' @param group_order the group order

.generate_additional_dataset_from_plotting_dataset <- function(plotting_dataset,
                                                               test.type = "t_test",
                                                               group_order = character())
{

  on.exit(gc())

  if (test.type == "t_test") {

    sub_plotting_dataset_ls <- split(plotting_dataset,by = "variable.name")

    seg_df_ls <- lapply(sub_plotting_dataset_ls,
                        JuJu:::.generate_additional_dataset_from_sub_plotting_dataset_for_t_test,
                        group_order = group_order)

    seg_df_na <- names(seg_df_ls)

    for (i in 1:length(seg_df_na)) {

      sub_seg_df <- seg_df_ls[[seg_df_na[i]]]
      sub_seg_df[,variable.name := seg_df_na[i]]

      seg_df_ls[seg_df_na[i]] <- list(sub_seg_df)

    }

  }

  return(rbindlist(seg_df_ls))

}

#' statistic geom_text
#'
#' @param data the data
#' @param test.type the test type
#' @export

geom_text_JuJu <- function(data,
                           test.type = "t_test")
{

  if (test.type == "t_test") {

    data <- data[geom_element == "significant_text"]
    data[,v_just := numeric()]
    data[text_label == "ns",v_just := -0.1]
    data[text_label != "ns",v_just := 0.4]

    data[,h_just := numeric()]
    data[text_label == "ns",h_just := -0.1]
    data[text_label != "ns",h_just := -0.05]

    geom_text_custom <- geom_text(data = data,
                                  aes(x = x_from,
                                      y = y_from,
                                      label = text_label,
                                      vjust = v_just,
                                      hjust = h_just),
                                  size = 4)

  }

  return(geom_text_custom)

}

#' generate additional dataset from sub plotting dataset for t test
#'
#' @param sub_plotting_dataset the sub plotting dataset
#' @param group_order the group order

.generate_additional_dataset_from_sub_plotting_dataset_for_t_test <- function(sub_plotting_dataset,
                                                                              group_order = character())
{

  on.exit(gc())

  main_generation_function <- function(comp_pairs,
                                       group_na,
                                       sub_plotting_dataset_by_group,
                                       sub_plotting_dataset_by_comparition){

    comp_pairs_sep <- JuJu:::.seperate_compared_pairs(comp_pairs = comp_pairs)
    seg_df <- lapply(comp_pairs_sep,
                     JuJu:::.conduct_loop_adding_to_plotting_dataset,
                     group_na = group_na,
                     max_d = length(comp_pairs_sep),
                     sub_plotting_dataset_by_group = sub_plotting_dataset_by_group,
                     sub_plotting_dataset_by_comparition = sub_plotting_dataset_by_comparition)

    seg_df_ls <- list()
    for (i in 1:length(seg_df)) {

      seg_df_ls <- append(seg_df_ls,seg_df[[i]])

    }

    seg_df <- rbindlist(seg_df_ls)

    return(seg_df)
  }

  sub_plotting_dataset_by_group <- unique(sub_plotting_dataset,by = "group") %>%
    setorder(-mean)
  sub_plotting_dataset_by_comparition <- unique(sub_plotting_dataset,by = "comparition")

  group_na <- sub_plotting_dataset_by_group[,group] %>%
    unique()

  if (length(group_order) == length(group_na)) {

    if (all(group_order %in% group_na)) {

      comp_pairs <- JuJu:::.create_group_pairs(group_na,
                                               annotate_interval = TRUE,
                                               group_order = group_order)

      seg_df <- main_generation_function(comp_pairs = comp_pairs,
                                         group_na = group_na,
                                         sub_plotting_dataset_by_group = sub_plotting_dataset_by_group,
                                         sub_plotting_dataset_by_comparition = sub_plotting_dataset_by_comparition)

    } else {

      comp_pairs <- JuJu:::.create_group_pairs(group_na,
                                               annotate_interval = TRUE,
                                               group_order = group_na)

      seg_df <- main_generation_function(comp_pairs = comp_pairs,
                                         group_na = group_na,
                                         sub_plotting_dataset_by_group = sub_plotting_dataset_by_group,
                                         sub_plotting_dataset_by_comparition = sub_plotting_dataset_by_comparition)

    }

  } else {

    comp_pairs <- JuJu:::.create_group_pairs(group_na,
                                             annotate_interval = TRUE,
                                             group_order = group_na)

    seg_df <- main_generation_function(comp_pairs = comp_pairs,
                                       group_na = group_na,
                                       sub_plotting_dataset_by_group = sub_plotting_dataset_by_group,
                                       sub_plotting_dataset_by_comparition = sub_plotting_dataset_by_comparition)

  }

  seg_df[,variable.name := as.character(unique(sub_plotting_dataset[,variable.name]))]

  return(seg_df)

}

#' seperate compared pairs
#'
#' @param comp_pairs the comparing pairs

.seperate_compared_pairs <- function(comp_pairs)
{

  comp_na <- names(comp_pairs)
  comp_pair_interval <- strsplit(comp_na,split = "-") %>%
    as.data.table()
  comp_pair_interval <- comp_pair_interval[3] %>%
    unlist() %>%
    unique() %>%
    paste("interval",sep = "-")
  pair_interval_ls <- JuJu:::.create_results_list(comp_pair_interval)

  for (i in 1:length(comp_pair_interval)) {

    single_interval <- comp_pair_interval[i] %>%
      strsplit(split = "-") %>%
      unlist()
    single_interval <- single_interval[1] %>%
      as.numeric()

    tool_ls <- list()
    for (j in 1:length(comp_na)) {

      comp_na_interval <- comp_na[j] %>%
        strsplit(split = "-") %>%
        unlist()
      comp_na_interval <- comp_na_interval[3] %>%
        as.numeric()

      if (single_interval == comp_na_interval) {

        tool_ls <- append(tool_ls,comp_pairs[comp_na[j]])

      }
    }

    tool_ls <- list(tool_ls)
    names(tool_ls) <- comp_pair_interval[i]
    pair_interval_ls[comp_pair_interval[i]] <- list(tool_ls)

  }

  return(pair_interval_ls)

}

#' conduct loop adding to plotting dataset
#'
#' @param comp_pairs_ls compared pairs list
#' @param group_na the group name
#' @param max_d the max distance
#' @param sub_plotting_dataset_by_group the sub plotting dataset unique by group
#' @param sub_plotting_dataset_by_comparition the sub plotting dataset unique by comparition

.conduct_loop_adding_to_plotting_dataset <- function(comp_pairs_ls,
                                                     group_na,
                                                     max_d,
                                                     sub_plotting_dataset_by_group,
                                                     sub_plotting_dataset_by_comparition)
{

  pair_ls_na <- names(comp_pairs_ls)
  loop_d <- strsplit(pair_ls_na,split = "-")
  loop_d <- loop_d[[1]][1] %>%
    as.numeric()

  if (loop_d == 1) {

    d = 0

  } else {

    d <- seq(from = (max_d - loop_d + 1),
             to = max_d) %>%
      sum()

    d <- d - (max_d - loop_d + 1)

  }

  comp_pairs <- comp_pairs_ls[[pair_ls_na]]
  comp_na <- names(comp_pairs)
  seg_df <- JuJu:::.create_results_list(comp_na)

  for (i in 1:length(group_na)) {

    d_comp_na <- character()
    for (j in 1:length(comp_na)) {

      sub_comp_pair <- comp_pairs[[comp_na[j]]] %>%
        unlist()

      if (group_na[i] %in% sub_comp_pair) {

        d <- d+1
        d_comp_na <- c(d_comp_na,comp_na[j])

        sub_comp_pair_ls <- list(paste(sub_comp_pair[1],sub_comp_pair[2],sep = "-"),
                                 paste(sub_comp_pair[2],sub_comp_pair[1],sep = "-"))

        another_group <- sub_comp_pair[!sub_comp_pair %in% group_na[i]]

        seg_df[comp_na[j]] <- data.table("geom_element" = c("hline",
                                                            "vline_greater",
                                                            "vline_less",
                                                            "significant_text"),
                                         "x_from" = c(sub_comp_pair[1],
                                                      sub_comp_pair[1],
                                                      sub_comp_pair[2],
                                                      sub_comp_pair[1]),
                                         "x_to" = c(sub_comp_pair[2],
                                                    sub_comp_pair[1],
                                                    sub_comp_pair[2],
                                                    sub_comp_pair[2]),
                                         "y_from" = c((sub_plotting_dataset_by_group[group == group_na[1],Q3])*(1+0.1*d),
                                                      (sub_plotting_dataset_by_group[group == group_na[1],Q3])*(1+0.1*d),
                                                      (sub_plotting_dataset_by_group[group == group_na[1],Q3])*(1+0.1*d),
                                                      (sub_plotting_dataset_by_group[group == group_na[1],Q3])*(1+0.1*d+0.01)),
                                         "y_to" = c((sub_plotting_dataset_by_group[group == group_na[1],Q3])*(1+0.1*d),
                                                    ((sub_plotting_dataset_by_group[group == sub_comp_pair[1],Q3]) + ((sub_plotting_dataset_by_group[group == group_na[1],Q3])*0.07)),
                                                    ((sub_plotting_dataset_by_group[group == sub_comp_pair[2],Q3]) + ((sub_plotting_dataset_by_group[group == group_na[1],Q3])*0.07)),
                                                    NA),
                                         "text_label" = c(NA,
                                                          NA,
                                                          NA,
                                                          as.character(unique(sub_plotting_dataset_by_comparition[comparition %in% sub_comp_pair_ls,sig_stars]))),
                                         "comparition" = rep(paste(sub_comp_pair[1],sub_comp_pair[2],sep = " vs "),times = 4)) %>%
          list()
      }
    }

    comp_na <- comp_na[!comp_na %in% d_comp_na]
    if (length(comp_na) == 0) {

      break

    }
  }

  return(seg_df)
}

#' t test barchart
#'
#' @param t_test_result_JuJu the t test result
#' @param group_order the group order
#' @param plotting_variable the plotting variable

t_test_barchart_JuJu <- function(t_test_result_JuJu,
                                 group_order,
                                 plotting_variable,
                                 aspect.ratio = sqrt(2),
                                 legend.position = "top",
                                 legend.direction = "horizontal",
                                 scales.free = "free_y",
                                 plot.facet = TRUE,
                                 expand.mult = sqrt(2),
                                 dodge.width = 0.8,
                                 bar.width = 0.6,
                                 error.width = 0.2,
                                 plot.title = "title",
                                 x_ticks = NULL,
                                 x_axis_angle = 25,
                                 x_axis_hjust = 1,
                                 panel.background = element_blank(),
                                 plot.background = element_blank(),
                                 plot.margin = margin(5,5,5,5),
                                 color_set = c("#6CBAD8","#96C2D4","#BAD2E1","#D8E5F7","#D8FEFA"),
                                 x_label = NULL,
                                 y_label = NULL)
{

  on.exit(gc())

  fill_colors <- color_set

  plotting_dataset <- JuJu:::.convert_statistic_result_to_plotting_dataset(statistic_result = t_test_result_JuJu)

  seg_df <- JuJu:::.generate_additional_dataset_from_plotting_dataset(plotting_dataset = plotting_dataset,
                                                                      group_order = group_order)

  if (plot.facet) {

    if (length(plot.title) == length(plotting_variable)) {

      plotting_dataset[,new_variable.name := "plot_title"]

      for (i in 1:length(plotting_variable)) {

        plotting_dataset[variable.name == plotting_variable[i],new_variable.name := plot.title[i]]
        seg_df[variable.name == plotting_variable[i],new_variable.name := plot.title[i]]

      }

      plotting_dataset[variable.name %in% plotting_variable,variable.name := new_variable.name]
      seg_df[variable.name %in% plotting_variable,variable.name := new_variable.name]
      plotting_variable <- plot.title

    }

    t_bar <- ggplot() +
      geom_bar(data = mutate(unique(plotting_dataset[variable.name %in% plotting_variable],
                                    by = "facet_group"),
                             group = factor(group,levels = group_order)),
               aes(x = group,
                   y = mean,
                   fill = group),
               stat = "identity",
               width = bar.width) +
      scale_fill_discrete(type = fill_colors[1:length(group_order)]) +
      geom_errorbar(data = mutate(unique(plotting_dataset[variable.name %in% plotting_variable],
                                         by = "facet_group"),
                                  group = factor(group,levels = group_order)),
                    aes(x = group,
                        ymin = Q2,
                        ymax = Q3),
                    width = error.width,
                    linewidth = 0.3) +
      geom_segment(data = seg_df[variable.name %in% plotting_variable & geom_element != "significant_text"],
                   aes(x = x_from,
                       xend = x_to,
                       y = y_from,
                       yend = y_to),
                   linewidth = 0.3) +
      JuJu::geom_text_JuJu(data = seg_df[variable.name %in% plotting_variable & geom_element == "significant_text"]) +
      scale_x_discrete(expand = expansion(mult = expand.mult)) +
      scale_y_continuous(expand = expansion(mult = c(0.01,0.1))) +
      facet_wrap(~variable.name,
                 scales = scales.free,
                 ncol = 2) +
      labs(fill = "Group",
           x = x_label,
           y = y_label) +
      theme_JuJu() +
      theme(aspect.ratio = aspect.ratio,
            legend.title = element_text(family = "Arial",
                                        size = unit(10,
                                                    units = "pt"),
                                        hjust = 0.5,
                                        margin = margin(5,5,5,5)),
            legend.position = legend.position,
            legend.direction = legend.direction,
            axis.text.x = element_text(angle = x_axis_angle,
                                       hjust = x_axis_hjust),
            legend.title.position = "top",
            plot.background = plot.background,
            plot.margin = plot.margin)

  } else {

    plotting_dataset[,plot_title := plot.title]

    sub_plotting_dataset <- mutate(unique(plotting_dataset[variable.name %in% plotting_variable],
                                          by = "facet_group"),
                                   group = factor(group,levels = group_order),
                                   variable.name = factor(variable.name,levels = plotting_variable))

    t_bar <- ggplot() +
      geom_bar(data = sub_plotting_dataset,
               aes(x = variable.name,
                   y = mean,
                   fill = group,
                   group = group),
               stat = "identity",
               width = bar.width,
               position = position_dodge(width = dodge.width)) +
      scale_fill_discrete(type = fill_colors[1:length(group_order)]) +
      geom_errorbar(data = sub_plotting_dataset,
                    aes(x = variable.name,
                        ymin = Q2,
                        ymax = Q3,
                        group = group),
                    width = error.width,
                    linewidth = 0.3,
                    position = position_dodge(width = dodge.width)) +
      # JuJu::geom_segment_JuJu(data = mutate(unique(plotting_dataset[variable.name %in% plotting_variable],
      #                                              by = "facet_group"),
      #                                       group = factor(group,levels = group_order)),
      #                         mapping = aes(x = variable.name,
      #                                       y = Q3,
      #                                       group = group,
      #                                       main.group = variable.name),
      #                         position = position_dodge(width = 0.6)) +
      scale_x_discrete(expand = expansion(mult = expand.mult),
                       labels = x_ticks) +
      scale_y_continuous(expand = expansion(mult = c(0.01,0.1))) +
      facet_wrap(~plot_title,
                 scales = scales.free,
                 ncol = 2) +
      labs(fill = "Group",
           x = x_label,
           y = y_label) +
      theme_JuJu() +
      theme(aspect.ratio = aspect.ratio,
            legend.title = element_text(family = "Arial",
                                        size = unit(10,
                                                    units = "pt"),
                                        hjust = 0.5,
                                        margin = margin(5,5,5,5)),
            legend.position = legend.position,
            legend.title.position = "top",
            legend.direction = legend.direction,
            axis.text.x = element_text(angle = x_axis_angle,
                                       hjust = x_axis_hjust),
            plot.background = plot.background,
            plot.margin = plot.margin)

  }

  return(t_bar)

}
