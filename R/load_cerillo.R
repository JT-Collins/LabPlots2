#' @export
load_cerillo <- function(filename, Time_int = 10) {



# Read in Cerillo data and map file ---------------------------------------

dat <- readxl::read_excel(filename,
                  sheet = "dat")

map <- readxl::read_excel(filename,
                  sheet = "map")



# Get into correct structure and join -------------------------------------

max_time <- (nrow(dat) -1) * Time_int # the total time of the experiment - the -1 accounts for the zero timepoint

df <-
  data.table::transpose(dat) |>
  dplyr::mutate(Well = colnames(dat), .before = V1) |>
  dplyr::left_join(map) |>
  janitor::clean_names(case = "upper_camel")
df
}
