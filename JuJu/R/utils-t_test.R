
# R/utils-t_test.R

#' conduct t test
#'
#' @param dataset_dt the data table
#' @param test_mode the test mode
#' @param group_col the column of group symbol
#' @param group_symbol_1 the group symbol 1
#' @param group_symbol_2 the group symbol 2
#' @param test_cols the columns to test
#' @export

conduct_t_test_JuJu <- function(dataset_dt,
                                group_col,
                                group_symbol_1,
                                group_symbol_2,
                                test_mode = "two.sided",
                                test_cols = character())
  {

  on.exit(gc())

  dataset_dt <- JuJu::add_new_col_to_data_table(original_data_table = dataset_dt,
                                                new_col = unlist(dataset_dt[,..group_col]),
                                                new_col_name = "group_symbol")

  if (length(test_cols) == 0) {

    test_cols <- JuJu:::.choose_test_cols(dt = dataset_dt)

  }

  test_result <- JuJu:::.create_results_list(test_cols)

  for (i in 1:length(test_cols)) {

    aim_col <- test_cols[i]

    dataset_dt <- JuJu::add_new_col_to_data_table(original_data_table = dataset_dt,
                                                  new_col = dataset_dt[,..aim_col],
                                                  new_col_name = "test_col")

    test_result[aim_col] <- list(t.test(dataset_dt[group_symbol == group_symbol_1,test_col],
                                        dataset_dt[group_symbol == group_symbol_2,test_col],
                                        alternative = test_mode,
                                        paired = FALSE,
                                        var.equal = FALSE,
                                        conf.level = 0.95))

  }

  t_value <- numeric()
  p_value <- numeric()
  conf_int_lower <- numeric()
  conf_int_upper <- numeric()
  mean_group_1 <- numeric()
  mean_group_2 <- numeric()
  standard_error <- numeric()
  test_method <- character()
  for (i in 1:length(test_cols)) {

    sub_test_result <- test_result[[test_cols[i]]]

    t_value <- c(t_value,sub_test_result$statistic)
    p_value <- c(p_value,sub_test_result$p.value)
    conf_int_lower <- c(conf_int_lower,sub_test_result$conf.int[1])
    conf_int_upper <- c(conf_int_upper,sub_test_result$conf.int[2])
    mean_group_1 <- c(mean_group_1,sub_test_result$estimate["mean of x"])
    mean_group_2 <- c(mean_group_2,sub_test_result$estimate["mean of y"])
    standard_error <- c(standard_error,sub_test_result$stderr)
    test_method <- c(test_method,sub_test_result$method)

  }

  test_result_dt <- data.table("variable.name" = test_cols,
                               "t.value" = t_value,
                               "p.value" = p_value,
                               "CI.lower" = conf_int_lower,
                               "CI.upper" = conf_int_upper,
                               "mean.group-1" = mean_group_1,
                               "mean.group-2" = mean_group_2,
                               "mean.diff" = (mean_group_1 - mean_group_2),
                               "group-1" = rep(group_symbol_1,times = length(test_cols)),
                               "group-2" = rep(group_symbol_2,times = length(test_cols)),
                               "st.err" = standard_error,
                               "test.method" = test_method)

  return(test_result_dt)

}

#' conduct t test
#'
#' @param test_dataset the data table
#' @param test_mode the test mode
#' @param group_col the column of group symbol
#' @export

conduct_t_test_each_JuJu <- function(test_dataset,
                                     group_col,
                                     test_mode = "two.sided")
  {

  on.exit(gc())

  test_dataset <- as.data.table(test_dataset)

  test_cols <- JuJu:::.choose_test_cols(dt = test_dataset)

  dataset_dt <- copy(test_dataset)

  dataset_dt <- JuJu::add_new_col_to_data_table(original_data_table = dataset_dt,
                                                new_col = dataset_dt[,..group_col],
                                                new_col_name = "group_symbol_copy")
  test_groups <- dataset_dt[,..group_col] %>%
    unique() %>%
    unlist()

  group_pairs <- JuJu:::.create_group_pairs(test_groups)
  group_pairs_na <- names(group_pairs)

  test_result <- JuJu:::.create_results_list(group_pairs_na)

  for (i in 1:length(group_pairs_na)) {

    gp_pa <- group_pairs[[i]]

    testing_cols <- character()
    for (j in 1:length(test_cols)) {

      sub_test_col <- test_cols[j]

      sub_dt <- dataset_dt[group_symbol_copy %in% gp_pa,..sub_test_col] %>%
        unlist()

      if (sum(is.na(sub_dt)) == 0) {

        testing_cols <- c(testing_cols,test_cols[j])

      }

    }

    if (length(testing_cols) != 0) {

      test_result[group_pairs_na[i]] <- JuJu::conduct_t_test_JuJu(dataset_dt = dataset_dt,
                                                                  test_cols = testing_cols,
                                                                  group_symbol_1 = group_pairs[[group_pairs_na[i]]][1],
                                                                  group_symbol_2 = group_pairs[[group_pairs_na[i]]][2],
                                                                  group_col = "group_symbol_copy") %>%
        list()

    }

  }

  cmb_res <- rbindlist(test_result)

  return(cmb_res)

}


#' plot t_test bar chart
#'
#' @param t_test_result the t_test result datatable
#' @export

plot_t_test_barchart_JuJu <- function(t_test_result)
  {

  on.exit(gc())

}
