#' Student'sche t-Verteilung
#'
#' Zeichnet die zentrale \eqn{t}-Verteilung für einen gegebenen Freiheitsgrad
#' und markiert die Beibehaltungs- und Verwerfungsbereiche der
#' Nullhypothese für ein- oder zweiseitige t-Tests. Zusätzlich wird
#' der empirische Prüfwert \eqn{t} als vertikale Linie eingezeichnet.
#'
#' @param W Numerischer Skalar. Empirischer \eqn{W}-Wert (z.B. aus einem
#'   \eqn{t}-Test oder Brunner–Munzel-Test).
#' @param df Numerischer Skalar. Freiheitsgrade der zugrunde liegenden
#'   \eqn{t}-Verteilung.
#' @param alpha Numerischer Skalar in \code{(0, 1)}. Signifikanzniveau,
#'   Standard ist \code{0.05}.
#' @param xmax Numerischer Skalar. Maximale absolute Ausdehnung der
#'   x-Achse; die Verteilung wird im Bereich \code{[-xmax, xmax]}
#'   dargestellt.
#' @param xlab_offset Numerischer Skalar. Horizontaler Versatz der
#'   Textannotation für den empirischen Wert \code{W}.
#' @param y_offset_factor Numerischer Skalar. Vertikale Position der
#'   Textannotation für \code{W} als Anteil der maximalen Dichte
#'   (z.B. \code{0.05} = 5 Prozent unterhalb der maximalen Höhe).
#' @param direction Zeichenkette, eine der Optionen `greater`,
#'   `less` oder `two.sided`. Bestimmt die Alternativhypothese
#'   und damit, ob der kritische Bereich im oberen, unteren oder in beiden
#'   Verteilungsschwänzen liegt.
#'
#' @return Ein \code{ggplot}-Objekt, das die t-Verteilung mit farbig
#'   markierten Beibehaltungs- und Verwerfungsbereichen sowie dem
#'   empirischen Prüfwert \code{W} darstellt.
#'
#' @examples
#' # einseitiger Test, Gruppe 2 > Gruppe 1
#' central.t.plot(W = 3.079201, df = 8, direction = "greater")
#'
#' # einseitiger Test, Gruppe 1 > Gruppe 2
#' central.t.plot(W = -3.079201, df = 8, direction = "less")
#'
#' # zweiseitiger Test
#' central.t.plot(W = 3.079201, df = 8, direction = "two.sided")
#'
#' @export
central.t.plot <- function(
  W,
  df,
  alpha = 0.05,
  xmax = 5,
  xlab_offset = 0.3,
  y_offset_factor = 0.05,
  direction = c("greater", "less", "two.sided")
) {
  direction <- base::match.arg(direction)

  range <- base::seq(-xmax, xmax, by = 0.01)
  dens <- stats::dt(range, df = df)

  if (direction == "greater") {
    t_crit_hi <- stats::qt(1 - alpha, df = df)
    df_plot <- tibble::tibble(
      x = range,
      y = dens,
      ymin = 0,
      ymax_reject = ifelse(x >= t_crit_hi, y, 0),
      ymax_retain = ifelse(x < t_crit_hi, y, 0)
    )
  } else if (direction == "less") {
    t_crit_lo <- stats::qt(alpha, df = df)
    df_plot <- tibble::tibble(
      x = range,
      y = dens,
      ymin = 0,
      ymax_reject = ifelse(x <= t_crit_lo, y, 0),
      ymax_retain = ifelse(x > t_crit_lo, y, 0)
    )
  } else {
    # two.sided
    t_crit_hi <- stats::qt(1 - alpha / 2, df = df)
    t_crit_lo <- stats::qt(alpha / 2, df = df)
    df_plot <- tibble::tibble(
      x = range,
      y = dens,
      ymin = 0,
      ymax_reject = ifelse(x <= t_crit_lo | x >= t_crit_hi, y, 0),
      ymax_retain = ifelse(x > t_crit_lo & x < t_crit_hi, y, 0)
    )
  }

  ymax <- base::max(dens)

  legend_df <- tibble::tibble(
    region = base::factor(
      c("Beibehalten", "Verwerfen"),
      levels = c("Beibehalten", "Verwerfen")
    ),
    x = c(Inf, Inf),
    y = c(Inf, Inf)
  )

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = df_plot,
      ggplot2::aes(x = x, ymin = ymin, ymax = ymax_retain),
      fill = "steelblue"
    ) +
    ggplot2::geom_ribbon(
      data = df_plot,
      ggplot2::aes(x = x, ymin = ymin, ymax = ymax_reject),
      fill = "#703342"
    ) +
    ggplot2::geom_point(
      data = legend_df,
      ggplot2::aes(x = x, y = y, colour = region)
    ) +
    ggplot2::scale_colour_manual(
      name = "\U1D43B\U2080",
      values = c("Beibehalten" = "steelblue", "Verwerfen" = "#703342")
    ) +
    ggplot2::geom_vline(xintercept = W, linewidth = 1) +
    ggplot2::annotate(
      "text",
      x = W + xlab_offset,
      y = ymax * (1 - y_offset_factor),
      label = base::paste0("\U1D461 = ", base::round(W, 3)),
      hjust = 0
    ) +
    ggplot2::labs(
      x = "\U1D461-Wert",
      y = "\U1D443(\U1D461)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5)
    )
}
