
# R/utils-shaprio_test.R

#' conduct shaprio test
#'
#' @param dataset_dt the dataset tabel
#' @param group_col the group col
#' @param test_col the testing col
#' @export

conduct_shaprio_test_JuJu <- function(dataset_dt,
                                      group_col,
                                      test_col)
{

  on.exit(gc())

  dataset_dt <- as.data.table(dataset_dt)

  gc <- dataset_dt[,..group_col]
  dataset_dt[,group := gc]

  gp <- dataset_dt[,group] %>%
    unlist() %>%
    unique()

  test_value_ls <- JuJu:::.create_results_list(test_col)
  for (i in 1:length(test_col)) {

    tc <- c("group",test_col[i])

    test_value_ls[test_col[i]] <- dataset_dt[,..tc] %>%
      list()

  }

  test_result <- JuJu:::.create_results_list(gp)
  test_col_ls <- as.list(test_col)
  names(test_col_ls) <- test_col
  for (i in 1:length(gp)) {

    sub_test_res <- lapply(test_col_ls,function(sub_test_col,group_symbol,test_value_ls){

      test_value <- test_value_ls[[sub_test_col]]
      test_value <- test_value[group == group_symbol,..sub_test_col] %>%
        unlist()

      if (length(test_value[!is.na(test_value)]) >= 3) {

        return(shapiro.test(test_value))

      } else {

        return(NULL)

      }

    },
    gp[i],
    test_value_ls)

    sub_test_res <- sub_test_res[lengths(sub_test_res) != 0]

    test_result[gp[i]] <- sub_test_res %>%
      list()

  }

  test_res_df <- list()
  gp_ls <- character()
  variable.name_ls <- character()
  for (i in 1:length(gp)) {

    sub_test_restult <- test_result[[gp[i]]]

    for (j in 1:length(sub_test_restult)) {

      test_res_df <- append(test_res_df,list(c(sub_test_restult[[j]]$statistic,
                                               sub_test_restult[[j]]$p.value)))

      gp_ls <- c(gp_ls,gp[i])
      variable.name_ls <- c(variable.name_ls,names(sub_test_restult)[j])

    }

  }

  test_res_df <- as.data.frame(test_res_df) %>%
    t()
  colnames(test_res_df) <- c("W","p.value")
  test_res_df <- test_res_df %>%
    as.data.table()

  test_res_df[,group := gp_ls]
  test_res_df[,variable.name := variable.name_ls]

  return(test_res_df)

}
