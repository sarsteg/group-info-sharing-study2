# Libraries ----------------------------------------------

library(psych)
library(dplyr)
library(tidyr)
library(haven)
library(descr)
library(ggplot2)
library(readr)

# Functions ----------------------------------------------





save_output_to_workbook <- function(wb, sheet_name, output) {
  
  # Remove sheet if it already exists
  if (sheet_name %in% names(wb)) {
    removeWorksheet(wb, sheet_name)
  }
  
  # Create worksheet
  addWorksheet(wb, sheet_name)
  
  # Try to convert output to a data frame
  output_df <- tryCatch(
    as.data.frame(output),
    error = function(e) NULL
  )
  
  # If conversion worked, save as a regular table
  if (!is.null(output_df)) {
    
    writeData(
      wb,
      sheet_name,
      output_df,
      rowNames = TRUE
    )
    
  } else {
    
    # If conversion failed, capture the printed output
    output_text <- capture.output(
      print(output)
    )
    
    # Save each printed line in its own row
    writeData(
      wb,
      sheet_name,
      data.frame(Output = output_text),
      rowNames = FALSE,
      colNames = FALSE
    )
  }
}

