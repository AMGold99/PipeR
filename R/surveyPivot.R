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
#' @param percent Do you want to view the results as group response rates to each selection? Defaults TRUE
#' @keywords engagement
#' @export
#' @examples
#' # Basic example with all defaults unchanged
#' \dontrun{
#' survey_data <- data.frame(ethnicity = c('black','black','white','white','white'),
#' experience = c('I did','did not','did not','I did','did not'))
#' surveyPivot(survey_data)
#'
#' # Adjusting defaults
#' surveyPivot(survey_data, group_column = 2, response_column = 1,
#' NAtoZero = FALSE, totals = FALSE, percent = FALSE)
#' }



####--------surveyPivot function-----------####

surveyPivot <- function(data, group_column = 1, response_column = 2, sep = ",", NAtoZero = TRUE, totals = TRUE, percent = TRUE) {

  # specify names of group variable (e.g., "Sex") and survey question, based on arguments
  group_variable <- names(data)[group_column]

  response_variable <- names(data)[response_column]

  # specify forbidden pattern
  forbidden_pattern <- paste0(sep, "[:blank:]")

  if(sum(stringr::str_detect(dplyr::pull(data, .data[[response_variable]]), pattern = forbidden_pattern), na.rm = TRUE)) {

    stop(paste0("Dataframe contains seperator (", sep, ") irregularly. Ensure the seperator only appears when separating responses. stringr::str_remove_all() may help here"))
  }

  if(length(names(data))>2) {

    stop("Dataframe is too wide. Ensure there are only two variables (group and response) before proceeding.")

  }

  if(group_column == response_column) {

    stop(paste0("Warning: group_column and response_column are identical. Both equal ", group_column))

  }





  # find max number of responses
  max_response <- max(stringr::str_count(tibble::deframe(data[,response_column]),"\\S,\\S"), na.rm = TRUE)

  # create vector of responses for response split
  responses <- paste('Response', seq_len(max_response), sep = " ")



  survey_pivot <- data %>%

    # split response column into one column for each response selected
    tidyr::separate(col = .data[[response_variable]],
                    into = responses,
                    sep = sep,
                    extra = "drop",
                    fill = "right") %>%

    # pivot responses so each row is a unique response
    tidyr::pivot_longer(
      cols = !.data[[group_variable]],
      names_to = "Response",
      values_to = "Value") %>%

    dplyr::group_by(.data[[group_variable]], Value) %>%

    dplyr::summarise(count = dplyr::n()) %>%

    dplyr::filter(!is.na(.data[[group_variable]]))


  # create totals column
  if(totals) {

    survey_pivot <- survey_pivot %>%

      tidyr::pivot_wider(
        names_from = .data[[group_variable]],
        values_from = count,
      ) %>%

      dplyr::filter(!is.na(Value))

    # replace NA values with 0
    if(NAtoZero==TRUE) {

      survey_pivot[is.na(survey_pivot)] <- 0

    }

    survey_pivot$Totals <- rowSums(survey_pivot[,2:ncol(survey_pivot)], na.rm = TRUE)



    survey_pivot <- survey_pivot %>%

      dplyr::filter(!is.na(Value)) %>%

      tidyr::pivot_longer(
        cols = !Value,
        names_to = group_variable,
        values_to = "count"
      )



  }


  if(percent) {

    # count total respondents by group
    ethnicity_count <- data %>%

      dplyr::select(.data[[group_variable]]) %>%

      dplyr::group_by(.data[[group_variable]]) %>%

      dplyr::summarise(total = dplyr::n()) %>%

      dplyr::filter(!is.na(.data[[group_variable]])) %>%

      dplyr::arrange(dplyr::desc(total))

    if(totals) {

      ethnicity_count <- ethnicity_count %>%

        rbind(., c('Totals',sum(.$total, na.rm = TRUE)))

    }



    # percentage table
    survey_final <- survey_pivot %>%

      dplyr::left_join(x = ., y = ethnicity_count) %>%

      dplyr::mutate(total = as.numeric(total),
                    percent_respond =  count / total) %>%

      dplyr::select(.data[[group_variable]], Value, percent_respond) %>%

      tidyr::pivot_wider(
        names_from = .data[[group_variable]],
        values_from = percent_respond
      ) %>%

      dplyr::filter(!is.na(Value))

  } else {

    # RAW COUNTS: group and aggregate by group and response (e.g., how many of Group x selected Response y)
    survey_final <- survey_pivot %>%

      # pivot wide so columns are groups and rows are responses
      tidyr::pivot_wider(
        names_from = .data[[group_variable]],
        values_from = count) %>%

      # remove NA response row
      dplyr::filter(!is.na(Value))

  }


  return(survey_final)

}
