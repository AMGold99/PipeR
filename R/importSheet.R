####-------Preamble---------####
library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)


importSheet <- function(link, email, sheet) {

  ss <- googlesheets4::as_sheets_id(link)

  googlesheets4::gs4_auth(email = email,
                          token = ss)

  data <- googlesheets4::read_sheet(
    ss = ss,
    sheet = sheet
  )

  return(data)

}
