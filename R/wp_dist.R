#' Exakte Verteilung von W-plus
#'
#' Plottet die exakte Verteilung von W-plus
#'
#' @param data dataframe mit der exakten Verteilung von W-plus
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_bar aes labs scale_y_continuous theme element_blank element_line element_text
#' @importFrom utils tail
#' @importFrom rlang .data
#' @export


wp_dist <- function(data) {

  if(data[1,2] != "Anz.") {
    warning("Datensatz scheint eine andere Datenstruktur aufzuweisen. Erwarte eine Haeufigkeitstabelle mit W\U0000208A in der ersten und Anz. in der zweiten Spalte.")
  }

  data <- data[1:2] |>
    utils::tail(-1)  |>
    `colnames<-`(c("wp", "freq")) |>
    dplyr::mutate(wp = as.numeric(.data$wp),
                  freq = as.numeric(.data$freq))

  ggplot2::ggplot(
    data,
    ggplot2::aes(x = factor(.data$wp),
                 y = .data$freq)) +
    ggplot2::geom_bar(stat = "identity",
                      fill = "steelblue") +
    ggplot2::labs(title = "Exakte Verteilung von W\U0000208A",
                  x = "W\U0000208A",
                  y = "Haeufigkeit") +
    ggplot2::scale_y_continuous(breaks = seq(0,
                                             max(data$freq),
                                             1)) +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_line(linewidth = .001, color = "grey"),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   text = ggplot2::element_text(size = 10),
                   axis.text.x = ggplot2::element_text(angle = 45))

}
