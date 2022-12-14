% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binom_plot.R
\name{binom.plot}
\alias{binom.plot}
\title{Binomialverteilung Plot}
\usage{
binom.plot(
  x,
  size,
  prob,
  tail = c("less", "greater", "two.sided"),
  cumul = c("point", "cumul"),
  scale = NULL
)
}
\arguments{
\item{x}{Empirisches x}

\item{size}{Stichprobengrösse}

\item{prob}{Auftretenswahrscheinlichkeit Pi}

\item{tail}{Indiziert die Richtung der Hypothese}

\item{cumul}{Indiziert, ob Punkt- oder kumulative Wahrscheinlichkeit ausgegeben werden soll}

\item{scale}{Benutzerdefinierte x-Achse}
}
\description{
Plottet die Binomialverteilung
}
