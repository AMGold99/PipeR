#' Collates dataframes of the form created by surveyPivot across multiple years
#'
#' Collates csv files from multiple years containing pivot tables created with surveyPivot. Variable names must be consistent across all files in the directory. The csv file names must contain their year (ex: "survey_2020.csv").
#' @title collate_csv
#' @param directory File path specifying the directory containing all the csv files to be collated.
#' @param group Character string specifying group variable name. Ex: "Ethnicity"
#' @param response Character string specifying response variable name. Defaults to "Value." Must refer to a consistent name across all pivot tables to be collated
#' @keywords read_csv collate time-series
#' @export



####--------------COLLATE FUNCTION--------------------####
collate_csv <- function(directory, group, response = "Value") {

  # create vector of paths to csv files
  paths <- fs::dir_ls(directory,
                      glob = paste0("*.csv"))

  # preallocate var name list
  name_list <- vector(mode = "list", length = length(paths))

  # create list of all var names in all csv files
  for(i in seq_len(length(paths))) {

    name_list[[i]] <- names(purrr::map(paths, ~ read.csv(.x))[[i]])

  }

  # check that all variable names are consistent across files
  if(!all(sapply(name_list, identical, name_list[[1]]))){
    print(name_list)
    stop("Variable names are not consistent across yearly csv files. \n See above.")
  }

  # collate csv files into one panel df
  timeseries <- purrr::map_dfr(paths,
                               ~ readr::read_csv(
                                 .x, id = "path",
                                 show_col_types = TRUE
                                 )
                               ) |>

    tidyr::extract(path, into = "year", "(\\d{4})") |>

    tidyr::pivot_longer(cols = !c(.data[[response]], year),
                        names_to = group,
                        values_to = "Amount") |>

    dplyr::mutate(year = as.integer(year))

  return(timeseries)

}






