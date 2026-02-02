
# R/utils-description_statistical_analysis.R

#' conduct description statistical analysis
#'
#' @param dataset_dt the dataset tabel
#' @param group_col the group col
#' @param test_col the testing col
#' @export

conduct_description_statistical_analysis_JuJu <- function(dataset_dt,
                                                          group_col,
                                                          test_col)
{

  on.exit(gc())

  dataset_dt <- as.data.table(dataset_dt)

  gc <- dataset_dt[,..group_col]
  dataset_dt[,group := gc]

  des_res <- JuJu:::.create_results_list(test_col)
  for (i in 1:length(test_col)) {

    tc <- test_col[i]
    tc <- dataset_dt[,..tc]
    dataset_dt[,test_col := tc]
    dataset_dt[!is.na(test_col)]

    sub_des_res <-     dataset_dt[!is.na(test_col)] %>%
      group_by(group) %>%
      summarise(Mean.value = mean(test_col),
                SD.value = sd(test_col)) %>%
      as.data.table()

    sub_des_res[,variable.name := test_col[i]]

    des_res[test_col[i]] <- list(sub_des_res)

  }

  return(rbindlist(des_res))

}
