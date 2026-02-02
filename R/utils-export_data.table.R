
# R/utils-export_data.table.R

#' export as excel
#'
#' @param dt_ls the data.table list
#' @param wb_na the workbook name

export_as_excel_JuJu <- function(dt_ls,
                                 wb_na,
                                 path,
                                 new = TRUE)
  {

  dt_na <- names(dt_ls)
  file_path <- paste(path,wb_na,sep = "/") %>%
    paste("xlsx",sep = ".")

  if (new) {

    wb <- createWorkbook()

  } else {

    wb <- loadWorkbook(file_path)

  }

  for (i in 1:length(dt_na)) {

    dt <- dt_ls[[dt_na[i]]]

    if (!is.data.table(dt)) {

      dt <- rbindlist(dt)

    }

    addWorksheet(wb,sheetName = dt_na[i])

    writeDataTable(wb,sheet = dt_na[i],x = dt)

  }

  saveWorkbook(wb,file = file_path,overwrite = TRUE)

  cat("save successfully")

}
