
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
                                test_cols = character(),
                                var.equal = FALSE,
                                paired = FALSE,
                                conf.level = 0.95,
                                normalize = TRUE,
                                simplify = FALSE)
  {

  on.exit(gc())

  dataset_dt <- JuJu::add_new_col_to_data_table(original_data_table = dataset_dt,
                                                new_col = unlist(dataset_dt[,..group_col]),
                                                new_col_name = "group_symbol")

  if (length(test_cols) == 0) {

    test_cols <- JuJu:::.choose_test_cols(dt = dataset_dt)

  }

  if (length(var.equal) != length(test_cols)) {

    test_col_str <- paste(test_cols,collapse = " ")
    var.equal <- rep(var.equal[1],
                     times = length(test_cols))

    cat("pairs:",group_symbol_1,group_symbol_2,"\n",sep = " ")
    cat("test_cols:",test_col_str,"\n",sep = " ")
    cat("var.equal = ",var.equal[1],"\n")

  } else {

    test_col_str <- paste(test_cols,collapse = " ")
    var.equal_str <- paste(var.equal,collapse = " ")
    cat("pairs:",group_symbol_1,group_symbol_2,"\n",sep = " ")
    cat("test_cols:",test_col_str,"\n",sep = " ")
    cat("var.equal:",var.equal_str,"\n",sep = " ")

  }

  test_result <- JuJu:::.create_results_list(test_cols)

  for (i in 1:length(test_cols)) {

    aim_col <- test_cols[i]

    dataset_dt <- JuJu::add_new_col_to_data_table(original_data_table = dataset_dt,
                                                  new_col = dataset_dt[,..aim_col],
                                                  new_col_name = "test_col")

    if (normalize) {

      test_result[aim_col] <- list(t.test(dataset_dt[group_symbol == group_symbol_1,test_col],
                                          dataset_dt[group_symbol == group_symbol_2,test_col],
                                          alternative = test_mode,
                                          paired = paired,
                                          var.equal = var.equal[i],
                                          conf.level = conf.level))

    } else {

      test_result[aim_col] <- list(wilcox.test(dataset_dt[group_symbol == group_symbol_1,test_col],
                                               dataset_dt[group_symbol == group_symbol_2,test_col],
                                               alternative = test_mode,
                                               paired = paired,
                                               conf.level = conf.level,
                                               conf.int = TRUE))

    }
  }

  t_value <- numeric()
  p_value <- numeric()
  conf_int_lower <- numeric()
  conf_int_upper <- numeric()
  mean_group_1 <- numeric()
  mean_group_2 <- numeric()
  standard_error <- numeric()
  test_method <- character()
  highest_group_1 <- numeric()
  highest_group_2 <- numeric()
  lowest_group_1 <- numeric()
  lowest_group_2 <- numeric()
  var.equal_ls <- logical()
  test.statistic <- character()
  for (i in 1:length(test_cols)) {

    aim_col <- test_cols[i]
    sub_test_result <- test_result[[test_cols[i]]]

    t_value <- c(t_value,sub_test_result$statistic)
    p_value <- c(p_value,sub_test_result$p.value)
    conf_int_lower <- c(conf_int_lower,sub_test_result$conf.int[1])
    conf_int_upper <- c(conf_int_upper,sub_test_result$conf.int[2])
    mean_group_1 <- c(mean_group_1,mean(unlist(dataset_dt[group_symbol == group_symbol_1,..aim_col])))
    mean_group_2 <- c(mean_group_2,mean(unlist(dataset_dt[group_symbol == group_symbol_2,..aim_col])))
    test_method <- c(test_method,sub_test_result$method)
    highest_group_1 <- c(highest_group_1,quantile(unlist(dataset_dt[group_symbol == group_symbol_1,..aim_col]),0.75))
    highest_group_2 <- c(highest_group_2,quantile(unlist(dataset_dt[group_symbol == group_symbol_2,..aim_col]),0.75))
    lowest_group_1 <- c(lowest_group_1,quantile(unlist(dataset_dt[group_symbol == group_symbol_1,..aim_col]),0.25))
    lowest_group_2 <- c(lowest_group_2,quantile(unlist(dataset_dt[group_symbol == group_symbol_2,..aim_col]),0.25))
    var.equal_ls <- c(var.equal_ls,var.equal[i])
    test.statistic <- c(test.statistic,names(sub_test_result$statistic))

    if (normalize) {

      standard_error <- c(standard_error,sub_test_result$stderr)

    }

  }

  if (normalize) {

    test_result_dt <- data.table("variable.name" = test_cols,
                                 "t.value" = t_value,
                                 "p.value" = p_value,
                                 "var.equal" = var.equal_ls,
                                 "CI.lower" = conf_int_lower,
                                 "CI.upper" = conf_int_upper,
                                 "mean.group-1" = mean_group_1,
                                 "mean.group-2" = mean_group_2,
                                 "mean.diff" = (mean_group_1 - mean_group_2),
                                 "group-1" = rep(group_symbol_1,times = length(test_cols)),
                                 "group-2" = rep(group_symbol_2,times = length(test_cols)),
                                 "st.err" = standard_error,
                                 "test.method" = test_method,
                                 "test.statistic" = test.statistic,
                                 "Q2.group-1" = lowest_group_1,
                                 "Q3.group-1" = highest_group_1,
                                 "Q2.group-2" = lowest_group_2,
                                 "Q3.group-2" = highest_group_2)

  } else {

    test_result_dt <- data.table("variable.name" = test_cols,
                                 "t.value" = t_value,
                                 "p.value" = p_value,
                                 "var.equal" = var.equal_ls,
                                 "CI.lower" = conf_int_lower,
                                 "CI.upper" = conf_int_upper,
                                 "mean.group-1" = mean_group_1,
                                 "mean.group-2" = mean_group_2,
                                 "mean.diff" = (mean_group_1 - mean_group_2),
                                 "group-1" = rep(group_symbol_1,times = length(test_cols)),
                                 "group-2" = rep(group_symbol_2,times = length(test_cols)),
                                 "test.method" = test_method,
                                 "test.statistic" = test.statistic,
                                 "Q2.group-1" = lowest_group_1,
                                 "Q3.group-1" = highest_group_1,
                                 "Q2.group-2" = lowest_group_2,
                                 "Q3.group-2" = highest_group_2)

  }

  if (simplify) {

    test_result_dt[,statistic.value := t.value]
    test_result_dt[,group := paste(`group-1`,`group-2`,sep = "-")]
    test_result_dt[,var.equal := var.equal_ls]

    test_result_dt <- test_result_dt[,c("variable.name",
                                        "statistic.value",
                                        "p.value",
                                        "test.statistic",
                                        "test.method",
                                        "group",
                                        "var.equal")]

    return(test_result_dt)

  } else {

    return(test_result_dt)

  }

}

#' conduct t test
#'
#' @param test_dataset the data table
#' @param test_mode the test mode
#' @param group_col the column of group symbol
#' @export

conduct_t_test_each_JuJu <- function(test_dataset,
                                     group_col,
                                     test_mode = "two.sided",
                                     test_cols = character(),
                                     var.equal = FALSE,
                                     paired = FALSE,
                                     conf.level = 0.95,
                                     normality = TRUE,
                                     simplify = FALSE)
  {

  on.exit(gc())

  test_dataset <- as.data.table(test_dataset)

  if (length(test_cols) == 0) {

    test_cols <- JuJu:::.choose_test_cols(dt = test_dataset)

  }

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
                                                                  group_col = "group_symbol_copy",
                                                                  var.equal = var.equal,
                                                                  paired = paired,
                                                                  conf.level = conf.level,
                                                                  normalize = normality,
                                                                  simplify = simplify) %>%
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
