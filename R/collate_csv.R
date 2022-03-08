collate_csv <- function(directory, group, response = "Value") {

  paths <- fs::dir_ls(directory,
                      glob = paste0("*.csv"))

  timeseries <- purrr::map_dfr(paths,
                               ~ readr::read_csv(
                                 .x, id = "path",
                                 show_col_types = FALSE
                                 )
                               ) |>

    tidyr::extract(path, into = "year", "(\\d{4})") |>

    tidyr::pivot_longer(cols = !c(.data[[response]], year),
                        names_to = group,
                        values_to = "Amount") |>

    dplyr::mutate(year = as.integer(year))

  return(timeseries)

}






