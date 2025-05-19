#' Funktion um alle offenen Antworten unten in den Anhang zu packen
#'
#' @param x Daten
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#' @param inkl.global Zweite inkl-Variable, die die globale Variable "inkl.open" abfragt. Kann auch in TRUE oder FALSE geändert werden
#' @param nr Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
#'
#' @examples
#' # Damit diese Funktion sinnvoll funktioniert, muss vorher mindestens eine
#' # offene Frage aufgerufen worden
#' invisible(capture.output(open.answers(BspDaten$dataSHOWUP$offen)))
#' appendix.open() |> markdown.in.viewer()
#'
#' @export appendix.open

appendix.open <- function() {
  
  anchor.nr <- list.open.answers$anchor.nr
  
  if (anchor.nr == 0)  {return()} # stoppen, wenn keine offenen Fragen aufgerufen wurden
  
  cat("# Anhang: Fragen mit offenem Antwortformat  \n  \n")
  
  for (k in seq_len(anchor.nr)) {
    x <- eval(parse(text = paste0("list.open.answers$var.", k)))
    q.nr <- eval(parse(text = paste0("list.open.answers$nr.", k)))
    merge.open(x, nr = q.nr, anchor = k)}
}