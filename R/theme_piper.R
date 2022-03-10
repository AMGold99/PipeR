#' Custom Piper Center ggplot theme
#'
#' Custom Piper Center visualization theme to add to ggplot objects. Built into pgplot (in-house panel plot format for Piper survey responses).
#' @title theme_piper
#' @export
#' @importFrom ggplot2 '%+replace%'

theme_piper <- function() {

  ggplot2::theme_grey() %+replace%

    ggplot2::theme(
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.6), hjust = 0, vjust = 2),
      plot.subtitle = ggplot2::element_text(hjust = 0, vjust = 1, margin = ggplot2::margin(t = 2, r = 0, b = 10, l = 0, unit = "pt")),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, colour = "black"),
      plot.margin = ggplot2::margin(t = 12, r = 12, b = 12, l = 12, unit = "pt"),
      legend.position = "right",
      strip.background = ggplot2::element_rect(fill = "#E4A01B", color = "#E4A01B")
    )

}
