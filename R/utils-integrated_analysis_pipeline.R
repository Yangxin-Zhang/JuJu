
# R/utils-integrated_analysis_pipeline.R

#' integrated_analysis_pipeline
#'
#' @param dataset_dt the dataset tabel
#' @param group_col the group col
#' @param test_col the testing col
#' @export

integrated_analysis_pipline_JuJu <- function(dataset_dt,
                                             group_col,
                                             test_col,
                                             export = FALSE,
                                             alpha = 0.05)
{

  on.exit(gc())

  dataset_dt <- as.data.table(dataset_dt)

  gc <- dataset_dt[,..group_col]
  dataset_dt[,group := gc]

  res_ls <- list()

  description <- JuJu::conduct_description_statistical_analysis_JuJu(dataset_dt = dataset_dt,
                                                                     group_col = group_col,
                                                                     test_col = test_col)

  res_ls <- append(res_ls,list(description))
  names(res_ls)[length(res_ls)] <- "description"
  cat("complete statistical description\n")

  normal_test <- JuJu::conduct_shaprio_test_JuJu(dataset_dt = dataset_dt,
                                                 group_col = group_col,
                                                 test_col = test_col)

  res_ls <- append(res_ls,list(normal_test))
  names(res_ls)[length(res_ls)] <- "normal_test"
  cat("complete normal test\n")

  res_ls_col <- JuJu:::.create_results_list(test_col)
  for (i in 1:length(test_col)) {

    sub_res_ls <- list()
    tc <- test_col[i]

    if (sum(normal_test[variable.name == tc,p.value] < 0.05) != 0) {

      normalize <- FALSE

    } else {

      normalize <- TRUE

    }

    if (length(unlist(normal_test[variable.name == tc,group])) > 2) {

      sub_res_ls <- c(sub_res_ls,JuJu:::.pipeline_ANOVE(dataset_dt = dataset_dt,
                                                group_col = group_col,
                                                test_col = tc,
                                                alpha = alpha,
                                                normality = normalize))

    } else {

      gp <- normal_test[variable.name == tc,group] %>%
        unlist()

      variable_col_value <- dataset_dt[,..tc]

      dataset_dt[,variable_col := variable_col_value]

      bartlett_res <- bartlett.test(variable_col~group,
                                    data = dataset_dt[!is.na(variable_col)])

      if (bartlett_res$p.value < 0.05) {

        var.equal <- FALSE

      } else {

        var.equal <- TRUE
      }

      bartlett_res_df <- data.table("variable.name" = test_col[i],
                                    "statistic.value" = bartlett_res$statistic,
                                    "p.value" = bartlett_res$p.value,
                                    "test.statistic" = names(bartlett_res$statistic),
                                    "test.method" =  bartlett_res$method,
                                    "group" = paste(unique(gp),collapse = "-"),
                                    "var.equal" = var.equal)

      t_test <- JuJu::conduct_t_test_JuJu(dataset_dt = dataset_dt,
                                          group_col = group_col,
                                          group_symbol_1 = gp[1],
                                          group_symbol_2 = gp[2],
                                          var.equal = var.equal,
                                          normalize = normalize,
                                          test_cols = test_col[i],
                                          simplify = TRUE)

      sub_res_ls <- append(sub_res_ls,list(rbindlist(list(t_test,bartlett_res_df))))
      names(sub_res_ls)[length(sub_res_ls)] <- "t_test"
      cat("complete t test")

    }
    res_ls_col[test_col[i]] <- list(sub_res_ls)
  }

  res_ls <- append(res_ls,res_ls_col)

  return(res_ls)

}

#' pipeline ANOVE
#'
#' @param dataset_dt the dataset tabel
#' @param group_col the group col
#' @param test_col the testing col

.pipeline_ANOVE <- function(dataset_dt,
                            group_col,
                            test_col,
                            alpha = 0.05,
                            normality = TRUE)
  {

  res_ls <- list()

  ANOVE_analysis <- JuJu::conduct_ANOVE_JuJu(dataset_dt = dataset_dt,
                                             group_col = group_col,
                                             test_col = test_col,
                                             normality = normality)

  res_ls <- append(res_ls,list(ANOVE_analysis))
  names(res_ls)[length(res_ls)] <- "ANOVE"
  cat("complete ANOVE\n")

  post_test_col <- ANOVE_analysis[test.statistic %in% c("F","Kruskal-Wallis chi-squared") & p.value < alpha,variable.name]

  if (length(post_test_col) != 0) {

    post_t_test <- JuJu::conduct_t_test_each_JuJu(test_dataset = dataset_dt,
                                                  group_col = group_col,
                                                  test_col = post_test_col,
                                                  var.equal = ANOVE_analysis[variable.name %in% post_test_col,var.equal],
                                                  normality = normality,
                                                  simplify = TRUE)

    res_ls <- append(res_ls,list(post_t_test))
    names(res_ls)[length(res_ls)] <- "post_t_test"
    cat("complete post t-test\n")

  }

  return(res_ls)

}
