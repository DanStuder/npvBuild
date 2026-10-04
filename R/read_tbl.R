#' Tabelle aus Excel einlesen (für Typst-Output wird die _typst-Version verwendet)
#'
#' @param filename Pfad relativ zum Projekt-Root, z.B. "Tables/binom1.xlsx"
#' @param html Wird HTML gerendert? Standard: knitr::is_html_output()
#' @export
read_tbl <- function(filename, html = knitr::is_html_output()) {
  if (!html) {
    filename <- stringr::str_replace(filename, "\\.xlsx$", "_typst.xlsx")
  }
  readxl::read_excel(here::here(filename))
}
