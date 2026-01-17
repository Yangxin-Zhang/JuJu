
# R/utils-general.R

#' add new col to data table
#'
#' @param original_data_table the original data table
#' @param new_col the new col
#' @param new_col_name the new col name
#' @export

add_new_col_to_data_table <- function(original_data_table,
                                      new_col,
                                      new_col_name)
  {

  on.exit(gc())

  original_data_table_copy <- copy(original_data_table)
  orgi_dt_cols <- colnames(original_data_table_copy)
  adj_orgi_dt_cols <- orgi_dt_cols[!orgi_dt_cols %in% new_col_name]

  tool_col <-  new_col %>%
    as.matrix()
  colnames(tool_col) <- new_col_name
  tool_col <- as.data.table(tool_col)

  new_data_table <- cbind(original_data_table_copy[,adj_orgi_dt_cols,with = FALSE],tool_col)

  return(new_data_table)

}

#' create results list
#'
#' @param res_na the result name

.create_results_list <- function(res_na)
  {

  on.exit(gc())

  res_ls <- vector("list",length = length(res_na))
  names(res_ls) <- res_na

  return(res_ls)

}

#' choose test cols
#' @param dt the datatable

.choose_test_cols <- function(dt)
  {

  col_nas <- colnames(dt)

  chosed_res <- select.list(choices = col_nas,
                            multiple = TRUE)

  return(chosed_res)

}

#' create group pairs
#'
#' @param group_list the group list

.create_group_pairs <- function(group_list)
  {

  on.exit(gc())

  group_pairs <- combn(group_list,2)

  pairs_na <- paste("pair",c(1:ncol(group_pairs)),sep = "-")
  res_pairs <- JuJu:::.create_results_list(pairs_na)
  for (i in 1:length(pairs_na)) {

    res_pairs[pairs_na[i]] <- list(group_pairs[,i])

  }

  return(res_pairs)

}

#' convert statistic result to plotting dataset
#'
#' @param statistic_result the statistic result in data.table form
#' @param test.type the test type

.convert_statistic_result_to_plotting_dataset <- function(statistic_result,
                                                          test.type = "t_test")
  {

  on.exit(gc())

  if (test.type == "t_test") {

    address_one_comparition <- function(plotting_dataset)
      {

      group_1 <- copy(plotting_dataset)
      group_2 <- copy(plotting_dataset)

      group_1[,sig_stars := JuJu:::.convert_p.value_to_significant_stars(p.value = group_1[,p.value],
                                                                         significant_level = 0.05)]
      group_2[,sig_stars := JuJu:::.convert_p.value_to_significant_stars(p.value = group_2[,p.value],
                                                                         significant_level = 0.05)]

      group_1[,mean := `mean.group-1`]
      group_2[,mean := `mean.group-2`]

      group_1[,group := `group-1`]
      group_2[,group := `group-2`]

      comp_na_1 <- paste(group_1[,`group-1`],group_2[,`group-2`],sep = "-")
      group_1[,comparition := comp_na_1]
      comp_na_2 <- paste(group_1[,`group-1`],group_2[,`group-2`],sep = "-")
      group_2[,comparition := comp_na_2]

      merge_group <- rbindlist(list(group_1,group_2))

      plotting_cols <- colnames(merge_group)[!colnames(merge_group) %in% c("mean.group-1","mean.group-2","group-1","group-2","st.err")]

      merge_group <- merge_group[,..plotting_cols]

      return(merge_group)

    }

    dt_ls <- split(statistic_result,seq_len(nrow(statistic_result)))

    result_ls <- lapply(dt_ls, address_one_comparition)

    plotting_dataset <- rbindlist(result_ls)

  }

  return(plotting_dataset)

}

#' convert p.value to significant stars
#'
#' @param p.value the p.value
#' @param significant_level the significant level of p.value

.convert_p.value_to_significant_stars <- function(p.value,
                                                   significant_level)
  {

  on.exit(gc())

  if (p.value > significant_level) {

    sig_stars <- "ns"

  } else {

    sci_format_p <- format(p.value, scientific = TRUE)
    sci_format_sig <- format(significant_level, scientific = TRUE)

    exp_part_p <- gsub(".*e([+-]?\\d+)", "\\1", sci_format_p) %>%
      as.numeric() %>%
      abs()
    exp_part_sig <- gsub(".*e([+-]?\\d+)", "\\1", sci_format_sig) %>%
      as.numeric() %>%
      abs()

    sig_stars <- rep("*",times = (exp_part_p-exp_part_sig+1)) %>%
      paste(collapse = "")

  }

  return(sig_stars)

}
