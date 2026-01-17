
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

      sub_plotting_dataset <- unique(sub_plotting_dataset,by = "group") %>%
        setorder(mean)

      group_na <- sub_plotting_dataset[,group] %>%
        unique()
      comp_pairs <- JuJu:::.create_group_pairs(group_na)
      comp_na <- names(comp_pairs)
      seg_df <- JuJu:::.create_results_list(comp_na)

      for (i in 1:length(group_na)) {

        d = 0
        for (j in 1:length(comp_na)) {

          sub_comp_pair <- comp_pairs[[comp_na[j]]]

          if (group_na[i] %in% sub_comp_pair) {

            d <- d+1

            another_group <- sub_comp_pair[!sub_comp_pair %in% group_na[i]]

            seg_df[comp_na[j]] <- data.table("line" = c("hline",
                                                        "vline_greater",
                                                        "vline_less"),
                                             "x_from" = c(group_na[i],
                                                          group_na[i],
                                                          another_group),
                                             "x_to" = c(another_group,
                                                        group_na[i],
                                                        another_group),
                                             "y_from" = c((sub_plotting_dataset[group == group_na[i],mean])*(1+0.1*d),
                                                          (sub_plotting_dataset[group == group_na[i],mean])*(1+0.1*d),
                                                          (sub_plotting_dataset[group == group_na[i],mean])*(1+0.1*d)),
                                             "y_to" = c((sub_plotting_dataset[group == group_na[i],mean])*(1+0.1*d),
                                                        (sub_plotting_dataset[group == group_na[i],mean])*1.05,
                                                        (sub_plotting_dataset[group == another_group,mean])*1.05),
                                             "comparition" = rep(paste(group_na[i],another_group,sep = " vs "),times = 3)) %>%
              list()
          }
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
