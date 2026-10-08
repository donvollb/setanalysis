# Ausgabe in Quarto-/R-Markdown-Dokumente -------------------------------

#' Abbildung oder Tabelle als eigenen Chunk ausgeben
#'
#' @description
#' In einem Chunk mit `output: asis` haben alle Abbildungen dieselbe Größe.
#' `subchunkify()` erzeugt deshalb für einen einzelnen Ausdruck (z. B. eine
#' Abbildung oder eine Tabelle) einen eigenen kleinen Chunk mit eigener
#' Abbildungsgröße, wertet ihn mit knitr aus und gibt das Ergebnis in den
#' Bericht aus. Alle Auswertungsfunktionen nutzen `subchunkify()` für ihre
#' Tabellen und Abbildungen.
#'
#' @param g Ausdruck, der eine Abbildung zeichnet oder eine Tabelle erzeugt.
#'   Er wird erst im Sub-Chunk ausgewertet. Mehrere Befehle können mit
#'   `c(...)` übergeben werden (dann `hide = TRUE` verwenden).
#' @param fig_height,fig_width Höhe und Breite der Abbildung in Zoll.
#' @param hide Bei `TRUE` wird nur die Abbildung ausgegeben und sonstige
#'   Ausgaben des Ausdrucks werden unterdrückt (Chunk-Option
#'   `results = "hide"`); bei `FALSE` wird alles wie in einem
#'   `asis`-Chunk ausgegeben.
#'
#' @returns Nichts (unsichtbar `NULL`). Das Ergebnis des Sub-Chunks wird mit
#'   [cat()] ausgegeben.
#'
#' @family werkzeuge
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' # Tabelle und Abbildung mit eigener Größe
#' subchunkify(lv_table(head(mtcars, 3)))
#' subchunkify(boxplot_grade(BspDaten$Plots$grade), fig_height = 2, fig_width = 9)
#' \dontshow{
#' setwd(.old_wd)
#' }
#'
#' @export
subchunkify <- function(g, fig_height = 7, fig_width = 5, hide = FALSE) {
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

#' Vorschau eines Berichtsabschnitts im Viewer
#'
#' @description
#' Zeigt das Ergebnis einer Auswertungsfunktion so an, wie es ungefähr im
#' Bericht aussehen wird: Die Ausgabe wird in HTML umgewandelt und im
#' RStudio-Viewer (bzw. im Browser) geöffnet. Praktisch zum Ausprobieren und
#' Testen; im Bericht selbst wird die Funktion nicht verwendet.
#'
#' Benötigt die zusätzlichen Pakete htmltools, markdown und svglite.
#'
#' @param markdown_function Aufruf einer Funktion, die Berichtscode ausgibt,
#'   z. B. `merge_sc(daten$frage)`.
#'
#' @returns Unsichtbar der Pfad zur erzeugten HTML-Datei; die Vorschau wird im
#'   Viewer geöffnet.
#'
#' @family werkzeuge
#'
#' @examples
#' if (interactive()) {
#'   markdown_in_viewer(merge_fachsem(BspDaten$dataLVE$FachSemN))
#' }
#'
#' @export
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

  # Einstellungen ändern und beim Verlassen der Funktion (auch bei einem
  # Fehler) auf die vorherigen Werte zurücksetzen --------------------------
  old_dev <- knitr::opts_chunk$get("dev")
  old_options <- options(knitr.duplicate.label = "allow") # vermeidet Fehlermeldungen
  on.exit({
    options(old_options)
    knitr::opts_chunk$set(dev = old_dev)
  })
  knitr::opts_chunk$set(dev = "svglite") # sorgt für richtige Plot-Darstellung

  # HTML Code für richtige Schriftart und Seitenbreite --------------------
  font_css <- "<style> body {font-family: 'Red Hat Text'} </style> \n\n"

  # Ergebnis, das normalerweise in die Konsole gedruckt wird, „abfangen“ --
  output_md <- capture.output(markdown_function)
  output_md <- paste(output_md, collapse = "\n")

  # Beides zusammenfügen --------------------------------------------------
  html_md <- paste(font_css, output_md)

  # Markdown im Viewer anzeigen --------------------------------------------
  markdown::mark_html(text = html_md, template = FALSE) |>
    htmltools::HTML() |>
    htmltools::html_print()
}
