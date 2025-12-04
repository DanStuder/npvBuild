#' Excel-Datei einlesen und als formatierte Huxtable-Tabelle ausgeben
#'
#' Diese Funktion liest eine Excel-Datei aus dem Unterordner "Tables" des Projektordners
#' ein. Die Datei wird ohne Spaltennamen importiert und in ein huxtable-Objekt umgewandelt
#' und grob formatiert. Es werden keine Spalten- oder Zeilennamen angezeigt, die
#' Spaltenbreite wird festgelegt, die Schriftart auf "Times New Roman" gesetzt, die
#' Schriftgrösse auf 14 und der Text zentriert.
#'
#' @param filename Zeichenkette. Name der Excel-Datei, die sich im Ordner "Tables"
#'   relativ zum Projektwurzelverzeichnis befindet.
#'
#' @return Ein huxtable-Objekt mit dem eingelesenen und formatierten Tabelleninhalt.
#'
#' @importFrom here here
#' @importFrom readxl read_excel
#' @importFrom huxtable as_hux set_col_width set_font set_font_size set_align
#' @importFrom utils globalVariables
#'
#' @export
#'
xl2hux <- function(filename) {
  here::here("Tables", filename) |>
    readxl::read_excel(
      .name_repair = "minimal",
      col_names = FALSE
    ) |>
    huxtable::as_hux(
      add_colnames = FALSE,
      add_rownames = FALSE,
      autoformat = FALSE
    ) |>
    huxtable::set_col_width(col = everywhere, 0.05) |>
    huxtable::set_font(row = everywhere, col = everywhere, "Times New Roman") |>
    huxtable::set_font_size(row = everywhere, col = everywhere, 14) |>
    huxtable::set_align(row = everywhere, col = everywhere, "center")
}
