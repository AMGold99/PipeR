#' Panel plot for surveys
#'
#' Creates multi-panel plot for survey response visualizations. Performs best when survey response strings are short and there are no more than 6 unique response values. (Name meaning: p for panel, g for gg, as in ggplot.)
#' @title pgplot
#' @param data Dataframe.
#' @param x Variable to be mapped on the x-axis
#' @param y Variable to be mapped on the y-axis
#' @param group Group variable (e.g., ethnicity), which will be delineated by color
#' @param facet Variable on which the plot panels (facets) will be based. Survey response is a sensible choice.
#' @param year Variable name denoting year. Defaults to "year".
#' @param title Plot title, as character string
#' @param subtitle Plot subtitle, as character string
#' @param ylab Label for y-axis, as character string
#' @param xlab Label for x-axis, as character string
#' @param caption Optional. Caption will be placed below plot.
#' @param palette palette from RColorBrewer. See \href{https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html}{here} for all palette options. Defaults to "Dark2".
#' @export


pgplot <- function(data, x, y, group, facet, year = "year",
                   title = "DEFAULT TITLE", subtitle = "DEFAULT subtitle",
                   ylab = "DEFAULT ylab", xlab = "DEFAULT xlab",
                   caption = NULL, palette = "Dark2") {

  if(length(unique_eval(data, col_name = facet))>6) {
    warning('Greater than 6 categories detected. Consider reducing the number of response categories.')
  }

  unique <- unique_eval(data, col_name = year)

  plot <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[x]], y = .data[[y]], colour = .data[[group]]))+

    ggplot2::geom_hline(yintercept = c(0, .25, .5, .75, 1),
               colour = "grey70")+

    ggplot2::geom_line()+

    ggplot2::geom_point(size = 2)+

    ggplot2::scale_x_continuous(
      breaks = unique,
      expand = c(0,0.25))+

    ggplot2::scale_y_continuous(
      limits = c(0,1),
      expand = c(0.05,0)) +

    ggplot2::facet_wrap( ~ .data[[facet]]) +

    ggplot2::labs(title = title,
         subtitle = subtitle,
         caption = caption) +

    ggplot2::ylab(ylab) +

    ggplot2::xlab(xlab) +

    ggplot2::scale_color_brewer(palette = palette)+

    theme_piper()

  return(plot)

}
