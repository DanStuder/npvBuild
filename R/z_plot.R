#' z-Verteilung Plot
#'
#' Plottet die z Verteilung
#'
#' @param z Empirisches z
#' @param tail Indiziert die Richtung der Hypothese
#' @param xmax Definiert Breite des Plots. 4 sieht meistens gut aus.
#' @param xlab Move the annotation on the x axis
#' @param ylab Move the annotation on the y axis
#' @importFrom ggplot2 ggplot aes geom_vline labs theme_minimal theme annotate element_text stat_function
#' @importFrom stats dnorm
#' @importFrom dplyr if_else
#' @export

z.plot = function(z, tail = c("less", "greater", "two.sided"), xmax = 4, xlab = (xmax/10), ylab = 0.001) {

  # Define some objects
  z.crit = switch (tail,
                   less = -1.645,
                   greater = 1.645,
                   two.sided = 1.96) # set z-crit

  x = c(-xmax, xmax)
  data = data.frame(x = x) # create the data

  # base plot with objects used in any case of "tail"
  base = ggplot(data = data, aes(x)) +

    # z-distribution
    stat_function(fun = dnorm,
                  geom = "area",
                  fill = "steelblue") +

    # vertical line at z
    geom_vline(xintercept = z,
               size = 1) +

    # print the z-value next to the vertical line
    annotate("text",
             x = z + xlab,
             y = 0.2 + ylab,
             label = paste0('z = ', round(z,4))) +

    # set axis labels
    labs(x = "z",
         y = "p") +
    theme_minimal() +
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5))


  # now the red part, that marks the significance, is added based on the value of z.crit
  if(z.crit == -1.645) {
    base + stat_function(fun = dnorm,
                         geom = "area",
                         fill = "#703342",
                         xlim = c(z.crit, -xmax))

  } else if (z.crit == 1.645){
    base + stat_function(fun = dnorm,
                         geom = "area",
                         fill = "#703342",
                         xlim = c(z.crit, xmax))
  } else {
    base +
      stat_function(fun = dnorm,
                    geom = "area",
                    fill = "#703342",
                    xlim = c(z.crit, xmax))+
      stat_function(fun = dnorm,
                    geom = "area",
                    fill = "#703342",
                    xlim = c(-z.crit, -xmax))
  }
}
