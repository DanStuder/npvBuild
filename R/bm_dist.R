#' Exakte verteilung von Brunner-Munzel's W
#'
#' Diese Funktion nimmt den Output von `npvTools::exact_bm_dist`
#' als Input und erstellt einen Plot der exakten Verteilung
#' @param data Output von `npvTools::exact_bm_dist`
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_bar aes labs theme element_blank element_line element_text
#' @importFrom utils globalVariables
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   test_data <- data.frame(
#'     value = c(0, 0, 1, 1, 1, 1, 1, 1, 3, 3),
#'     group = factor(rep(1:2, each = 5))
#'   )
#'   real_data <- npvTools::exact_bm_dist(test_data)
#'   bm_dist(real_data)
#' }
#'
bm_dist <- function(data) {

  data |>
    dplyr::mutate(W = base::round(W, 3)) |>
    ggplot2::ggplot(ggplot2::aes(
                    x = base::factor(W),
                    y = n_perm)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::labs(title = "Exakte Verteilung von \U1D44A",
                  x = "\U1D44A",
                  y = "H\U00E4ufigkeit") +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_line(linewidth = .001, color = "grey"),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   text = ggplot2::element_text(size = 10))

}
