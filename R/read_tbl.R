#' Tabelle aus Excel einlesen (für Typst-Output wird die _typst-Version verwendet)
#'
#' @param filename Pfad relativ zum Projekt-Root, z.B. "Tables/binom1.xlsx"
#' @param html Wird HTML gerendert? Standard: knitr::is_html_output()
#' @export
read_tbl <- function(filename, html = knitr::is_html_output()) {
  if (!html) {
    typst_file <- stringr::str_replace(filename, "\\.xlsx$", "_typst.xlsx")
    if (file.exists(here::here(typst_file))) filename <- typst_file
  }
  readxl::read_excel(here::here(filename))
}
