#' Unique values of column in tidy dataframe
#'
#' Low-level function not recommended for end-user usage (see base R unique function). Generates vector of unique values from a column of a tidy dataframe using tidy eval principles
#' @title unique_eval
#' @param data Tidy dataframe.
#' @param col_name Character string specifying the column whose unique values will be pulled.
#' @export



####--------------UNIQUE_EVAL FUNCTION--------------------####
unique_eval <- function(data, col_name) {

  unique <- data |>
    dplyr::count(.data[[col_name]]) |>
    dplyr::pull(.data[[col_name]])

  return(unique)
}
