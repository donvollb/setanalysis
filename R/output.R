# Ausgabe in Quarto-/R-Markdown-Dokumente -------------------------------

#' Erzeugt Subchunks für die Berichte
#'
#' @param g Code (kann auch mit Aufzählung ("c(...)") benutzt werden)
#' @param fig_height Höhe des Sub-Chunks
#' @param fig_width Breite des Sub-Chunks
#' @param hide Soll für die results-Option des Chunks "asis" verwendet werden?
#'
#' @returns Subchunk
#'
#' @export


# Erzeugen von Sub-Chunks
subchunkify <- function(g, # Code (kann auch mit Aufzählung ("c(...)") benutzt werden)
                        fig_height = 7, # Höhe des Sub-Chunks
                        fig_width = 5, # Breite des Sub-Chunks
                        hide = FALSE) # Soll für die results-Option des Chunks "asis" verwendet werden?
{
  g_deparsed <- paste0(deparse(
    function() {
      g
    }
  ), collapse = "")

  if (hide == FALSE) {
    head.end <- ", echo=FALSE, results = \"asis\", fig.align = \"center\", out.width = \"100%\"}"
  } else {
    head.end <- ", echo=FALSE, results = \"hide\", fig.keep = \"all\", fig.align = \"center\", out.width = \"100%\"}"
  }

  if (!exists("sub.nr")) {
    assign("sub.nr", 0, envir = globalenv())
  }
  assign("sub.nr", sub.nr + 1, envir = globalenv())

  sub_chunk <- paste0(
    "```{r sub_chunk_", sub.nr, ", fig.height=", fig_height, ", fig.width=", fig_width, head.end,
    "  \npar(family = \"", setanalysis_defaults$font.family, "\")  \n",
    "  \n",
    "\n(",
    "  \n",
    g_deparsed,
    ")()",
    "\n```"
  )

  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
}

# Eigene Funktion zum Runden
true_round <- function(number, digits) {
  posneg <- sign(number)
  number <- abs(number) * 10^digits
  number <- number + 0.5 + sqrt(.Machine$double.eps)
  number <- trunc(number)
  number <- number / 10^digits
  number * posneg
}

#' Funktion um den Markdown-Code, welcher durch eine Funktion erzeugt wurde
#' direkt im Viewer anzuzeigen
#'
#' @param markdown_function Funktion, welche Markdown-Code erzeugt
#'
#' @description Diese Funktion ist dafür gedacht, die anderen Funktionen, die
#' Markdown-Code mit [cat()] direkt in die Konsole drucken zu testen, sie ist
#' nicht für die Verwendung in einem Dokument gedacht.
#'
#' `markdown.in.viewer()` ist eine veraltete Schreibweise der gleichen Funktion
#'
#' @returns Vorschau im Viewer, wie das Endergebnis im Dokument aussehen würde
#' @export
#'
#' @examples markdown_in_viewer(merge.fachsem(BspDaten$dataLVE$FachSemN))
markdown_in_viewer <- function(markdown_function) {
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
