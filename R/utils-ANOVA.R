
# R/utils-ANOVA.R

#' conduct ANOVE test
#'
#' @param dataset_dt the dataset table
#' @param group_col the group col
#' @param test_col the testing col
#' @export

conduct_ANOVE_JuJu <- function(dataset_dt,
                               group_col,
                               test_col,
                               normality = TRUE)
  {

  on.exit(gc())

  dataset_dt <- as.data.table(dataset_dt)

  gc <- dataset_dt[,..group_col] %>%
    unlist()
  dataset_dt[,group := gc]

  test_result <- JuJu:::.create_results_list(test_col)
  test_result_var <- JuJu:::.create_results_list(test_col)
  var.equal_ls <- logical()
  for (i in 1:length(test_col)) {

    tc <- test_col[i]

    variable_col_value <- dataset_dt[,..tc]

    dataset_dt[,variable_col := variable_col_value]

    bartlett_res <- bartlett.test(variable_col~group,
                                  data = dataset_dt)

    if (bartlett_res$p.value < 0.05) {

      var.equal <- FALSE


      cat(test_col[i],"var.equal = FALSE\n",sep = " ")

    } else {

      var.equal <- TRUE

      cat(test_col[i],"var.equal = TURE\n",sep = " ")

    }

    if (normality) {

      anove_res <- oneway.test(variable_col~group,
                               data = dataset_dt,
                               var.equal = var.equal)

    } else {

      anove_res <- kruskal.test(variable_col~group,
                               data = dataset_dt)

    }

    var.equal_ls <- c(var.equal_ls,var.equal)

    test_result[test_col[i]] <- data.table("variable.name" = test_col[i],
                                           "statistic.value" = anove_res$statistic,
                                           "p.value" = anove_res$p.value,
                                           "test.statistic" = names(anove_res$statistic),
                                           "test.method" = anove_res$method,
                                           "group" = paste(unique(gc),collapse = "-")) %>%
      list()
    test_result_var[test_col[i]] <- data.table("variable.name" = test_col[i],
                                               "statistic.value" = bartlett_res$statistic,
                                               "p.value" = bartlett_res$p.value,
                                               "test.statistic" = names(bartlett_res$statistic),
                                               "test.method" =  bartlett_res$method,
                                               "group" = paste(unique(gc),collapse = "-")) %>%
      list()

  }

  test_result_df <- rbindlist(test_result)

  test_result_df[,var.equal := var.equal_ls]

  test_result_df_var <- rbindlist(test_result_var)

  test_result_df_var[,var.equal := var.equal_ls]

  return(bind_rows(test_result_df,test_result_df_var))

}
