
# R/utils-t_test_barchart.R

#' generate segment dataset from plotting dataset
#'
#' @param plotting_dataset the plotting dataset
#' @param test.type the test type

.generate_segment_dataset_from_plotting_dataset <- function(plotting_dataset,
                                                            test.type = "t_test")
  {

  on.exit(gc())

  if (test.type == "t_test") {

    address_one_comparition <- function(sub_plotting_dataset){

      sub_plotting_dataset_by_group <- unique(sub_plotting_dataset,by = "group") %>%
        setorder(-mean)
      sub_plotting_dataset_by_comparition <- unique(sub_plotting_dataset,by = "comparition")

      group_na <- sub_plotting_dataset_by_group[,group] %>%
        unique()
      comp_pairs <- JuJu:::.create_group_pairs(group_na)
      comp_na <- names(comp_pairs)
      seg_df <- JuJu:::.create_results_list(comp_na)

      d = 0
      for (i in 1:length(group_na)) {

        d_comp_na <- character()
        for (j in 1:length(comp_na)) {

          sub_comp_pair <- comp_pairs[[comp_na[j]]]

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
                                             "x_from" = c(group_na[i],
                                                          group_na[i],
                                                          another_group,
                                                          group_na[i]),
                                             "x_to" = c(another_group,
                                                        group_na[i],
                                                        another_group,
                                                        another_group),
                                             "y_from" = c((sub_plotting_dataset_by_group[group == group_na[1],mean])*(1+0.1*d),
                                                          (sub_plotting_dataset_by_group[group == group_na[1],mean])*(1+0.1*d),
                                                          (sub_plotting_dataset_by_group[group == group_na[1],mean])*(1+0.1*d),
                                                          (sub_plotting_dataset_by_group[group == group_na[1],mean])*(1+0.1*d+0.01)),
                                             "y_to" = c((sub_plotting_dataset_by_group[group == group_na[1],mean])*(1+0.1*d),
                                                        (sub_plotting_dataset_by_group[group == group_na[i],mean])*1.05,
                                                        (sub_plotting_dataset_by_group[group == another_group,mean])*1.05,
                                                        NA),
                                             "text_label" = c(NA,
                                                              NA,
                                                              NA,
                                                              as.character(unique(sub_plotting_dataset_by_comparition[comparition %in% sub_comp_pair_ls,sig_stars]))),
                                             "comparition" = rep(paste(group_na[i],another_group,sep = " vs "),times = 4)) %>%
              list()
          }
        }

        comp_na <- comp_na[!comp_na %in% d_comp_na]
        if (length(comp_na) == 0) {

          break

        }

      }

      seg_df <- rbindlist(seg_df)
      seg_df[,variable.name := as.character(unique(sub_plotting_dataset[,variable.name]))]

      return(seg_df)

    }

    sub_plotting_dataset_ls <- split(plotting_dataset,by = "variable.name")

    seg_df_ls <- lapply(sub_plotting_dataset_ls, address_one_comparition)

  }

  return(seg_df_ls)

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
    data[text_label == "ns",v_just := 0]
    data[text_label != "ns",v_just := 0.5]

    geom_text_custom <- geom_text(data = data,
                                  aes(x = x_from,
                                      y = y_from,
                                      label = text_label,
                                      vjust = v_just),
                                  hjust = 0,
                                  size = 8)

  }

  return(geom_text_custom)

}
