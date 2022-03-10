

unique_eval <- function(data, col_name) {

  #col_name <- dplyr::enquo(col_name)

  unique <- data |>
    dplyr::count(.data[[col_name]]) |>
    dplyr::pull(.data[[col_name]])

  return(unique)
}
