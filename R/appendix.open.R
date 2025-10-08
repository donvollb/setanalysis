#' Funktion um alle offenen Antworten unten in den Anhang zu packen
#'
#' @param freq Sollen die offenen Antworten nach Häufigkeit gruppiert werden?
#'
#' @examples
#' # Damit diese Funktion sinnvoll funktioniert, muss vorher mindestens eine
#' # offene Frage aufgerufen worden
#' invisible(capture.output(open.answers(BspDaten$dataSHOWUP$offen)))
#' appendix.open() |> markdown_in_viewer()
#'
#' @export appendix.open

appendix.open <- function(freq = FALSE) {
  
  anchor.nr <- list.open.answers$anchor.nr
  
  if (anchor.nr == 0)  {return(invisible())} # stoppen, wenn keine offenen Fragen aufgerufen wurden
  
  cat("# Anhang: Fragen mit offenem Antwortformat  \n  \n")
  
  for (k in seq_len(anchor.nr)) {
    x <- eval(parse(text = paste0("list.open.answers$var.", k)))
    q.nr <- eval(parse(text = paste0("list.open.answers$nr.", k)))
    merge.open(x, nr = q.nr, anchor = k, freq = freq)}
}