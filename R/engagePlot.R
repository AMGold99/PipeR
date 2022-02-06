#' Engagement Plot Function
#'
#' Plots engagement-type-specific engagement over all four class years by academic major.
#' @title engagePlot
#' @param data Dataframe of with class year, major, and individual student engagement tallies for events (e_attend), coach (coach_appt), peer advisor (pa_appt), and coach or peer advisor (c_pa_appt)
#' @param major Major of interest (character). Must match major spelling and case exactly as found in data
#' @param etype Engagement type (character). "Coach or Peer Advisor", "Coach", "Peer Advisor", or "Events"
#' @param save Do you wish to save the plot output to the session directory? Default is FALSE
#' @param save_dir If save==TRUE, specify what directory (or file path to that directory, omitting the top-level working directory) in which you wish to save the file
#' @keywords engagement
#' @export
#' @examples
#' #engagePlot(engage_df, "Mathematics", "Coach", save=TRUE, save_dir="piper")
#' #engagePlot(engage_df, "Mathematics", "Coach or Peer Advisor", save=TRUE, save_dir = file.path("piper","engagement"))


####---Engagement Graph Function---####



engagePlot <- function(data, major, etype, save=FALSE, save_dir=NULL) {

  if (!is.element(major,data$maj_conc)) {

    stop("Warning: Major misspecified. For complete list of majors, enter: unique(df$maj_conc)")

  }

  #subset only major of interest ("major" argument)
  minterest <- data %>%
    dplyr::filter(class_year == "Freshman" | class_year == "Sophomore" | class_year == "Junior" | class_year == "Senior") %>%
    dplyr::filter(maj_conc == major) %>%
    dplyr::group_by(class_year) %>%
    dplyr::filter(class_year != "NA") %>%
    dplyr::mutate(class_year = forcats::fct_relevel(class_year, c("Freshman","Sophomore","Junior","Senior")))

  #agg engagement type of interest ("etype" argument) across indivs from maj of interest
  if (etype == "Coach or Peer Advisor") {

    minterest <- minterest %>%
      dplyr::summarise(enkind = sum(c_pa_appt))

  } else if (etype == "Coach") {

    minterest <- minterest %>%
      dplyr::summarise(enkind = sum(coach_appt))

  } else if (etype == "Peer Advisor") {

    minterest <- minterest %>%
      dplyr::summarise(enkind = sum(pa_appt))

  } else if (etype == "Events") {

    minterest <- minterest %>%
      dplyr::summarise(enkind = sum(e_attend))

  } else {

    stop(
      "Warning: engagement type misspecified. Please enter one of the following: 'Coach or Peer Advisor', 'Coach','Peer Advisor','Events'"
    )

  }

  #create df for all majors over time
  exper <- data %>%
    dplyr::filter(class_year == "Freshman" | class_year == "Sophomore" | class_year == "Junior" | class_year == "Senior") %>%
    dplyr::filter(maj_conc != "Undeclared" & maj_conc != "Undeclared PSEO") %>%
    dplyr::mutate(class_year = forcats::fct_relevel(class_year, c("Freshman","Sophomore","Junior","Senior"))) %>%
    dplyr::group_by(maj_conc, class_year)

  if (etype == "Coach or Peer Advisor") {

    exper1 <- exper %>%
      dplyr::summarise(enkind = sum(c_pa_appt))

  } else if (etype == "Coach") {

    exper1 <- exper %>%
      dplyr::summarise(enkind = sum(coach_appt))

  } else if (etype == "Peer Advisor") {

    exper1 <- exper %>%
      dplyr::summarise(enkind = sum(pa_appt))

  } else if (etype == "Events") {

    exper1 <- exper %>%
      dplyr::summarise(enkind = sum(e_attend))

  } else {

    stop(
      "Warning: engagement type misspecified. Please enter one of the following: 'Coach or Peer Advisor', 'Coach','Peer Advisor','Events'"
    )

  }
  #plot all majors over time, with major of interest highlighted (geom_line(minterest))
  plot <- ggplot2::ggplot(exper1, ggplot2::aes(x = class_year, y = enkind))+

    ggplot2::geom_hline(yintercept = c(0,100,200,300,400),size = 0.25,color="#ececec")+

    ggplot2::geom_line(ggplot2::aes(group = maj_conc), color = "#d2d2d2", size = 0.75)+

    ggplot2::geom_point(data=subset(exper1,class_year != "Sophomore" & class_year != "Junior" & maj_conc != major),color="#d2d2d2",size=2)+

    ggplot2::geom_line(data=minterest,ggplot2::aes(group=1),size=1,color="#1696d2")+
    ggplot2::geom_point(data=minterest,size=3,color="#1696d2", shape=21, fill="white")+

    ggrepel::geom_text_repel(
      data=prob::subset(exper1, class_year == "Senior" & maj_conc == major),
      ggplot2::aes(class_year, enkind, label=major),
      nudge_x=0.2,
      color = "#1696d2",
      size = 4.2)+

    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)))+

    ggplot2::labs(title="Student Engagement with Piper Center",
                  subtitle = paste(major, "engagement over time:",tolower(etype)),
                  x = NULL,
                  y = paste(etype,"(#)"))+

    cowplot::theme_minimal_hgrid()

  if (save == TRUE) {

    #create "piperfile" file path to save plot
    root <- file.path(getwd(),save_dir)

    filemajor <- paste(stringr::str_sub(tolower(gsub(" ", "", major)),1,8),"_",stringr::str_sub(tolower(gsub(" ", "",etype)),1,8),"_plot.png",sep="")

    piperfile <- file.path(root,filemajor)


    #save plot in 'piper' folder
    ggplot2::ggsave(piperfile, width = 15, height = 10, units = "cm", bg = "white")

  }


  return(plot)

}
