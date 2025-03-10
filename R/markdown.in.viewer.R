#' Funktion um den Markdown-Code, welcher durch eine Funktion erzeugt wurde
#' direkt im Viewer anzuzeigen
#'
#' @param markdown_function Funktion, welche Markdown-Code erzeugt 
#'
#' @description Diese Funktion ist dafür gedacht, die anderen Funktionen, die
#' Markdown-Code mit [cat()] direkt in die Konsole drucken zu testen, sie ist
#' nicht für die Verwendung in einem Dokument gedacht.
#'
#' @returns Vorschau im Viewer, wie das Endergebnis im Dokument aussehen würde
#' @export
#'
#' @examples markdown.in.viewer(merge.fachsem(BspDaten$dataLVE$FachSemN))

markdown.in.viewer <- function(markdown_function) {
  
  # Bisherige Bildoptionen speichern um sie später wiederherzustellen ----
  image.device <- knitr::opts_chunk$get("dev")
  
  # Einstellungsänderungen ------------------------------------------------
  options(knitr.duplicate.label = "allow") # vermeidet Fehlermeldungen
  knitr::opts_chunk$set(dev = "svglite") # sorgt für richtige Plot-Darstellung
  
  # HTML Code für richtige Schriftart und Seitenbreite --------------------
  font.code <- "<style> body {font-family: 'Red Hat Text'</style> \n\n"

  # Ergebnis, das normalerweise in die Konsole gedruckt wird, „abfangen“ --
  code.result <- capture.output(markdown_function)
  code.result <- paste(code.result, collapse = "\n")
  
  # Beides zusammenfügen --------------------------------------------------
  merge <- paste(font.code, code.result)

  # Markdown im Viewer anzeigen --------------------------------------------
  markdown::mark_html(text = merge, template = FALSE) |>
    htmltools::HTML() |>
    htmltools::html_print()
  
  # Einstellungen zurücksetzen --------------------------------------------
  options(knitr.duplicate.label = "forbid")
  knitr::opts_chunk$set(dev = image.device)
}
