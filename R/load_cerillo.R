#' load_cerillo
#'
#' @param filename The excel file that contains your OD600 readings in the 'dat' sheet and the variable data on the 'map' sheet
#'
#' @param Time_int Time between readings. Defaults to 10 mins
#'
#' @export
load_cerillo <- function(filename, Time_int = 10, range = "F:CW") {
  # Check if required packages are installed and load the packages
  required_packages <- c("readxl", "janitor", "dplyr", "tidyr", "data.table", "cellranger", "zoo")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    install_commands <- paste("install.packages('", missing_packages, "')", sep = "")
    stop(paste("The following packages are required:", paste(missing_packages, collapse = ", "),
               "please install using", paste(install_commands, collapse = " or ")))
  }

  # Read in Cerillo data and map file ---------------------------------------

  dat <- readxl::read_excel(filename,
                            sheet = "dat",
                            skip = 10,
                            range = cellranger::cell_cols(range))

  map <- readxl::read_excel(filename,
                            sheet = "map") |>
    janitor::remove_empty(which = "cols") |> # Drop columns with nothing in
    dplyr::mutate(dplyr::across(tidyselect::everything(), as.character))



  # Get into correct structure and join -------------------------------------

  max_time <- (nrow(dat)) * Time_int-1 # the total time of the experiment - the -1 accounts for the zero timepoint

  df <-
    data.table::transpose(dat) |>
    dplyr::mutate(Well = colnames(dat), .before = V1) |>
    dplyr::left_join(map) |>
    janitor::clean_names(case = "upper_camel") |>
    #mutate(across(any_of(factor_names), as.factor)) |> # convert plate numbers etc to factors
    #na.omit() |>
    tidyr::pivot_longer(cols = where(is.double),
                        names_to = "Time",
                        values_to = "OD600") |>
    dplyr::mutate(Time = rep(seq(0,max_time, by = Time_int), times = ncol(dat))) %>%
    group_by(across(c(-Time, -OD600))) %>%
    mutate(Med = zoo::rollmedian(OD600, k = 3, fill='extend'),
           Smooth = zoo::rollmean(OD600, k = 3, fill='extend')) %>%
    ungroup()


  df_summary <-
    df %>%
    group_by(across(c(-Well, -OD600, -Med, -Smooth))) %>%
    summarise(mean_OD = mean(OD600),
              median_OD = median(OD600),
              sd_OD = sd(OD600),
              IQR_OD = IQR(OD600),
              mad_OD = mad(OD600),
              max_OD = max(OD600),
              S_mean_OD = mean(Smooth),
              S_median_OD = median(Smooth),
              S_sd_OD = sd(Smooth),
              S_IQR_OD = IQR(Smooth),
              S_mad_OD = mad(Smooth),
              S_max_OD = max(Smooth),
              reps = n()) %>%
    ungroup()

  # Create list of data frame using list()
  return(list(df, df_summary))



}
