#' Imports data from a specified Google Sheets spreadsheet
#'
#' Collates csv files from multiple years containing pivot tables created with surveyPivot. Variable names must be consistent across all files in the directory. The csv file names must contain their year (ex: "survey_2020.csv").
#' @title importSheet
#' @param link Link to Google Sheets document containing the data. May be a full url or a Google Sheets ID (the string between "../d/" and "/edit..." in the url).
#' @param email Email address which has access to the Google Sheets document.
#' @param sheet Character string specifying the name of the spreadsheet within the Google Sheets document which contains the desired data.
#' @param range Optional. Character string specifying the range to be imported (e.g., 'A1:G100').
#' @param col_names Defaults to TRUE, using the first row in the spreadsheet as the column names. A character vector of custom column names may be passed to this argument.
#' @param ... Other read_sheet arguments. See ?read_sheet for more.
#' @keywords googlesheets4 import
#' @export

importSheet <- function(link, email, sheet, range=NULL, col_names = TRUE, ...) {

  ss <- googlesheets4::as_sheets_id(link)

  googlesheets4::gs4_auth(email = email,
                          token = ss)

  data <- googlesheets4::read_sheet(
    ss = ss,
    sheet = sheet,
    range = range,
    col_names = col_names,
    ...
  )

  return(data)

}
