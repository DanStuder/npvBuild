#' z-Verteilung Plot
#'
#' Plottet die z Verteilung
#'
#' @param emp Empirisches z
#' @param mean Mittelwert der Verteilung
#' @param var Varianz der Verteilung
#' @param tail Indiziert die Richtung der Hypothese
#' @param xmax Definiert Breite des Plots. 4 sieht meistens gut aus.
#' @param xlab Move the annotation on the x axis
#' @param ylab Move the annotation on the y axis
#' @importFrom ggplot2 ggplot aes geom_vline labs theme_minimal theme annotate element_text stat_function scale_x_continuous element_blank
#' @importFrom stats dnorm
#' @importFrom dplyr if_else
#' @importFrom scales number_format
#' @export

# z.plot = function(z, tail = c("less", "greater", "two.sided"), xmax = 4, xlab = (xmax/10), ylab = 0.001) {

norm.plot = function(
  emp,
  mean,
  var,
  tail = c("less", "greater", "two.sided"),
  xlab = (xmax / 10),
  ylab = 0.001
) {
  # Define some objects
  sd = sqrt(var)
  norm.crit = switch(
    tail,
    less = mean - 1.645 * sd,
    greater = mean + 1.645 * sd,
    two.sided = c((100 - 1.96 * sd), (100 + 1.96 * sd))
  ) # set z-crit

  xmax = mean + 5 * sd
  xmin = mean - 5 * sd

  x = c(xmin, xmax)
  data = data.frame(x = x)

  # base plot with objects used in any case of "tail"
  base = ggplot(data.frame(x = x), aes(x)) +
    stat_function(fun = dnorm, args = list(mean, sd)) +
    geom_vline(xintercept = emp, size = 1) +
    annotate(
      "text",
      x = emp + xlab,
      y = 0.05 + ylab,
      label = paste0('emp = ', round(emp, 4))
    ) +
    # set axis labels
    labs(x = "norm", y = expression(italic(p))) +
    theme_minimal() +
    theme(
      axis.title.y = element_text(angle = 0, vjust = 0.5),
      panel.grid.minor = element_blank()
    )

  base

  # base = ggplot(data = data, aes(x)) +
  #
  #   # z-distribution
  #   stat_function(fun = dnorm,
  #                 geom = "area",
  #                 fill = "steelblue") +
  #
  #   # vertical line at z
  #   geom_vline(xintercept = z,
  #              size = 1) +
  #
  #   # print the z-value next to the vertical line
  #   annotate("text",
  #            x = z + xlab,
  #            y = 0.2 + ylab,
  #            label = paste0('z = ', round(z,4))) +
  #   scale_x_continuous(breaks = c(seq(-xmax, 2.5, by = 0.5),
  #                                 seq(-1.5, 1.5, by = 0.5),
  #                                 seq(2.5, xmax, by = 0.5)),
  #                      labels = number_format(accuracy = 0.1)) +
  #
  #   # set axis labels
  #   labs(x = "z",
  #        y = "p") +
  #   theme_minimal() +
  #   theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
  #         panel.grid.minor = element_blank())
  #
  #
  # # now the red part, that marks the significance, is added based on the value of z.crit
  # if(z.crit == -1.645) {
  #   base + stat_function(fun = dnorm,
  #                        geom = "area",
  #                        fill = "#703342",
  #                        xlim = c(z.crit, -xmax))
  #
  # } else if (z.crit == 1.645){
  #   base + stat_function(fun = dnorm,
  #                        geom = "area",
  #                        fill = "#703342",
  #                        xlim = c(z.crit, xmax))
  # } else {
  #   base +
  #     stat_function(fun = dnorm,
  #                   geom = "area",
  #                   fill = "#703342",
  #                   xlim = c(z.crit, xmax))+
  #     stat_function(fun = dnorm,
  #                   geom = "area",
  #                   fill = "#703342",
  #                   xlim = c(-z.crit, -xmax))
  # }
}
