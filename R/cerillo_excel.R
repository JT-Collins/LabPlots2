#' @param filename The excel file that contains your OD600 readings in the 'dat' sheet and the variable data on the 'map' sheet
#'
#' @export

cerillo_excel <- function(filename = paste0(Sys.Date(), ".xlsx")) {

  well_coordinates <- expand.grid(row = LETTERS[1:8], col = 1:12)
  well_coordinates <-  well_coordinates[
    with(well_coordinates, order(row, col)),
  ]
  well_coordinates$well <- paste0(well_coordinates$row, well_coordinates$col)

  # Create a workbook
  wb <- openxlsx::createWorkbook()

  # Add the "dat" sheet and write the well coordinates
  openxlsx::addWorksheet(wb, "map")
  openxlsx::writeData(wb, "map", x = c("Well", well_coordinates$well), startCol = 1, startRow = 1)

  # Add the "map" sheet
  openxlsx::addWorksheet(wb, "dat")

  # Save the workbook to a file
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)

}
