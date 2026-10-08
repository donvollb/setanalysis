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
subchunkify <- function(g, # Code (kann auch mit Aufzählung ("c(...)") benutzt werden)
                        fig_height = 7, # Höhe des Sub-Chunks
                        fig_width = 5, # Breite des Sub-Chunks
                        hide = FALSE) # Soll für die results-Option des Chunks "asis" verwendet werden?
{
  # Code in eine Funktion verpacken: `g` wird so erst im Sub-Chunk ausgewertet
  # und knitr erfasst die dabei entstehenden Abbildungen mit eigener Größe
  g_deparsed <- paste0(deparse(
    function() {
      g
    }
  ), collapse = "")

  results_option <- if (hide == FALSE) {
    'results = "asis"'
  } else {
    'results = "hide", fig.keep = "all"'
  }

  # Fortlaufende Nummer für eindeutige Chunk-Namen
  .subchunk_env$counter <- .subchunk_env$counter + 1

  sub_chunk <- paste0(
    "```{r sub_chunk_", .subchunk_env$counter,
    ", fig.height=", fig_height, ", fig.width=", fig_width,
    ", echo=FALSE, ", results_option,
    ', fig.align = "center", out.width = "100%"}\n',
    'par(family = "', setanalysis_defaults$font.family, '")\n',
    "(", g_deparsed, ")()\n",
    "```"
  )

  # Sub-Chunk knitten; knit() wertet ihn in dieser Funktion aus (dort ist `g`
  # bekannt). Hinweis: knitr::knit_child() ist hier kein gleichwertiger Ersatz,
  # es gibt Abbildungen außerhalb eines Dokuments anders aus.
  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
}

# Zähler für die Namen der Sub-Chunks (sub_chunk_1, sub_chunk_2, …)
.subchunk_env <- new.env(parent = emptyenv())
.subchunk_env$counter <- 0

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
  # Zusätzlich benötigte Pakete prüfen (nur für diese Vorschau nötig) -----
  required <- c("htmltools", "markdown", "svglite")
  missing_pkgs <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("Für markdown_in_viewer() werden zusätzlich folgende Pakete benötigt: ",
      paste(missing_pkgs, collapse = ", "), "\n",
      "Installation mit: install.packages(c(\"", paste(missing_pkgs, collapse = "\", \""), "\"))",
      call. = FALSE
    )
  }

  # Bisherige Bildoptionen speichern um sie später wiederherzustellen ----
  old_dev <- knitr::opts_chunk$get("dev")

  # Einstellungsänderungen ------------------------------------------------
  options(knitr.duplicate.label = "allow") # vermeidet Fehlermeldungen
  knitr::opts_chunk$set(dev = "svglite") # sorgt für richtige Plot-Darstellung

  # HTML Code für richtige Schriftart und Seitenbreite --------------------
  font_css <- "<style> body {font-family: 'Red Hat Text'</style> \n\n"

  # Ergebnis, das normalerweise in die Konsole gedruckt wird, „abfangen“ --
  output_md <- capture.output(markdown_function)
  output_md <- paste(output_md, collapse = "\n")

  # Beides zusammenfügen --------------------------------------------------
  html_md <- paste(font_css, output_md)

  # Markdown im Viewer anzeigen --------------------------------------------
  markdown::mark_html(text = html_md, template = FALSE) |>
    htmltools::HTML() |>
    htmltools::html_print()

  # Einstellungen zurücksetzen --------------------------------------------
  options(knitr.duplicate.label = "forbid")
  knitr::opts_chunk$set(dev = old_dev)
}
