#' Binomialverteilung Plot
#'
#' Plottet die Binomialverteilung
#'
#' @param x Empirisches x
#' @param size Stichprobengrösse
#' @param prob Auftretenswahrscheinlichkeit Pi
#' @param tail Indiziert die Richtung der Hypothese
#' @param cumul Indiziert, ob Punkt- oder kumulative Wahrscheinlichkeit ausgegeben werden soll
#' @importFrom ggplot2 geom_bar aes theme element_text
#' @importFrom stats dbinom
#' @export


binom.plot = function(x, size, prob, tail = c("less", "greater", "two.sided"), cumul = c("point", "cumul")) {

  # Warnings
  if(prob != 0.5 && tail == "two.sided"){
    stop("Plotting of two-tailed distribution currently only works reliabily for probability = 0.5!")
  }

  # Generate a random sample
  n <- seq(0, size, by = 1)

  # Calculate probabilities
  p <- dbinom(x = n, size = size, prob = prob)

  # Merge data sets
  binom_data <- data.frame(n, p)


  # Colors
  colores = rep("steelblue", size+1) # creates vector of "steelblue" with the length of `size`

  # length of cumulation -> how many bars need to be colored on each side
  if(x <= (size/2)){
    l = length(0:x)
  } else {l = length(size:x)}

  less = seq(from = 1, by = 1, length.out = l) # sequence starting from 1, increasing by 1 and length of l
  greater = seq(size+1, by = -1, length.out = l)
  two.sided = c(less, greater)

  colvec = switch(cumul,
                  "point" = x+1,
                  "cumul" = switch(tail,
                                   "less" = less,
                                   "greater" = less,
                                   "two.sided" = two.sided
                  ))
  colvec

  colores[colvec] = "#703342" # overwrite the elements of colores that are in colvec with a dark red

  # Plot data
  ggplot(data = binom_data) +
    geom_bar(aes(x = n,
                 y = p),
             stat = "identity",
             fill = colores) +
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
          text = element_text(size = 15))

}
