#' Extrahiert bei MC-Fragen das Label der Antwortoption
#'
#' @param x Eine Spalte.
#' @param match Die Zeichen, die direkt vor der Antwortoption stehen (üblicherweise ´: ´)
#' @returns Eine Zeichenkette (das Label).


# Labels für MC-Fragen aus dem Fragetext ziehen:
get.label <- function(x, # Objekt
                      match = ": ") # String, der Label von Frage trennt
{
  sub(paste0(".*", match), '', attr(x, "label"))
}
