#' Survey Pivot Function
#'
#' Creates pivot table from survey data containing irregular multiple responses. Input is clean dataframe containing a group variable (e.g., race/ethnicity) and a response variable (selected responses to a survey question).
#' @title surveyPivot
#' @param data Tidy dataframe (ideally of two columns) containing group variable (e.g., ethnicity) and response variable where each row is a unique respondent.
#' @param group_column Column of the dataframe where the group variable resides. Defaults to 1.
#' @param response_column Column of the dataframe where the response variable resides. Defaults to 2.
#' @param sep Do you wish to save the plot output to the session directory? Default is FALSE
#' @param NAtoZero Do you want NA values in the pivot table to be replaced with 0? Defaults to TRUE
#' @param totals Do you want a column containing row sums across all groups? Defaults to TRUE.
#' @keywords engagement
#' @export
#' @examples
#' # Basic example with all defaults unchanged
#' surveyPivot(survey_data)
#'
#' # Adjusting defaults
#' surveyPivot(survey_data, group_column = 2, response_column = 1, NAtoZero = FALSE, totals = FALSE)



####--------surveyPivot function-----------####

surveyPivot <- function(data, group_column = 1, response_column = 2, sep = ",", NAtoZero = TRUE, totals = TRUE) {

  if(length(names(data))>2) {

    stop("Dataframe is too wide. Ensure there are only two variables (group and response) before proceeding.")

  }

  if(group_column == response_column) {

    stop(paste0("Warning: group_column and response_column are identical. Both equal ", group_column))

  }

  # specify names of group variable (e.g., "Sex") and survey question, based on arguments
  group_variable <- names(data)[group_column]

  survey_variable <- names(data)[response_column]



  # find max number of responses
  max_response <- max(stringr::str_count(tibble::deframe(data[,response_column]),"\\S,\\S"), na.rm = TRUE)

  # create vector of responses for response split
  responses <- paste('Response', seq_len(max_response), sep = " ")



  survey_pivot <- data %>%

    # split response column into one column for each response selected
    tidyr::separate(col = .data[[survey_variable]],
                    into = responses,
                    sep = sep,
                    extra = "drop",
                    fill = "right") %>%

    # pivot responses so each row is a unique response
    tidyr::pivot_longer(
      cols = !.data[[group_variable]],
      names_to = "Response",
      values_to = "Value") %>%

    # group and aggregate by group and response (e.g., how many of Group x selected Response y)
    dplyr::group_by(.data[[group_variable]], Value) %>%

    dplyr::summarise(count = dplyr::n()) %>%

    # pivot wide so columns are groups and rows are responses
    tidyr::pivot_wider(
      names_from = .data[[group_variable]],
      values_from = count) %>%

    # remove NA response row
    dplyr::filter(!is.na(Value))



  # replace NA values with 0
  if(NAtoZero==TRUE) {

    survey_pivot[is.na(survey_pivot)] <- 0

  }



  # create totals column
  if(totals==TRUE) {

    survey_pivot$Totals <- rowSums(survey_pivot[,2:ncol(survey_pivot)])

    survey_pivot <- survey_pivot %>%
      dplyr::arrange(plyr::desc(Totals))

  }


  return(survey_pivot)

}
