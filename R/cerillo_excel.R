#' cerillo_excel
#'
#' @param filename The excel file that contains your OD600 readings in the 'dat' sheet and the variable data on the 'map' sheet
#'
#' @export


cerillo_excel <- function(filename = paste0(Sys.Date(), ".xlsx")) {

  # cerillo_excel: Function to create an Excel file with well coordinates and empty data sheets
  # filename: Name of the Excel file to be created (default: current date.xlsx)

  # Validate filename input
  if (!is.character(filename)) {
    stop("The 'filename' parameter should be a character string.")
  }

  # Check if required packages are installed and load the package
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("The 'openxlsx' package is required to use this function. Please install it using install.packages('openxlsx').")
  }
  library(openxlsx)

  # Create well coordinates
  well_coordinates <- expand.grid(row = LETTERS[1:8], col = 1:12)
  well_coordinates <- well_coordinates[with(well_coordinates, order(row, col)), ]
  well_coordinates$well <- paste0(well_coordinates$row, well_coordinates$col)

  # Create a workbook
  wb <- createWorkbook()

  # Add the "map" sheet and write the well coordinates
  addWorksheet(wb, "map")
  writeData(wb, "map", x = c("Well", well_coordinates$well), startCol = 1, startRow = 1)
  writeData(wb, "map", x = "Biol_rep", startCol = 2, startRow = 1, colNames = FALSE)

  # Add the "dat" sheet
  addWorksheet(wb, "dat")

  # Save the workbook to a file
  tryCatch(
    saveWorkbook(wb, filename, overwrite = TRUE),
    error = function(e) {
      stop("An error occurred while saving the workbook. Please check the filename and try again.")
    }
  )
}
