#' Plot der exakten Verteilung von S (Kendall-Test)
#'
#' Diese Funktion erstellt ein Balkendiagramm, das die Häufigkeitsverteilung
#' der Teststatistik S für den Kendall-Test anzeigt.
#'
#' @param data Ein Dataframe oder eine Matrix mit mindestens zwei Spalten,
#'   welches im Skript verwendet wird, um die Häufigkeitstabelle zu erstellen.
#'   Die erste Spalte muss die Werte der Statistik S enthalten,
#'   die zweite Spalte die zugehörigen Häufigkeiten.
#'
#' @return Ein ggplot2-Objekt, das das Balkendiagramm darstellt.
#' @export
#'
#' @importFrom dplyr slice mutate across everything
#' @importFrom ggplot2 ggplot aes geom_col theme_minimal theme element_text element_blank labs scale_y_continuous
#' @importFrom stats setNames
#'

kendall.plot <- function(data) {

  # Datenvorbereitung
  plot_data <- data[1:2] |>
    dplyr::slice(-1) |>
    stats::setNames(c("val", "freq")) |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), as.numeric),
      val = factor(val)
    )

  # Plot erzeugen
  ggplot2::ggplot(data = plot_data, ggplot2::aes(x = val, y = freq)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.title.y = ggplot2::element_text(vjust = 0.5),
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 15)
    ) +
    ggplot2::labs(
      title = expression("Exakte Verteilung von" ~ italic(S)),
      x = expression(italic(S)),
      y = expression("Häufigkeit")
    ) +
    ggplot2::scale_y_continuous(breaks = seq(0, max(plot_data$freq), 1))
}
