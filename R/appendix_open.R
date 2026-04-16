#' Funktion um alle offenen Antworten unten in den Anhang zu packen
#' `appendix.open()` ist eine veraltete Schreibweise der gleichen Funktion
#'
#' @param freq Sollen die offenen Antworten nach Häufigkeit gruppiert werden?
#'
#' @examples
#' # Damit diese Funktion sinnvoll funktioniert, muss vorher mindestens eine
#' # offene Frage aufgerufen worden
#' invisible(capture.output(merge_open(BspDaten$dataSHOWUP$offen, appendix = TRUE)))
#' appendix_open() |> markdown_in_viewer()
#'
#' @export appendix_open

appendix_open <- function(freq = "auto") {
  
  anchor.nr <- list.open.answers$anchor.nr
  
  if (anchor.nr == 0)  {return(invisible())} # stoppen, wenn keine offenen Fragen aufgerufen wurden
  
  cat("# Anhang: Fragen mit offenem Antwortformat  \n  \n")
  
  for (k in seq_len(anchor.nr)) {
    x <- eval(parse(text = paste0("list.open.answers$var.", k)))
    q.nr <- eval(parse(text = paste0("list.open.answers$nr.", k)))
    merge_open(x, nr = q.nr, anchor = k, freq = freq,
               appendix = TRUE, is_appendix = TRUE)}
}

#' @noRd
#' @export

appendix.open <- appendix_open