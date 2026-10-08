#' Erstes und zweites Fach gemeinsam auswerten
#'
#' @description
#' Fasst zwei Single-Choice-Fragen mit denselben Antwortoptionen zusammen,
#' typischerweise „Was ist Ihr 1. Fach?“ und „Was ist Ihr 2. Fach?“ im
#' 2-Fach-Bachelor, und wertet sie wie [merge_sc()] in einer gemeinsamen
#' Tabelle aus. Ein Hinweis unter der Tabelle erklärt, dass sich dadurch der
#' Stichprobenumfang verdoppelt.
#'
#' @param x1,x2 Antworten auf die Frage nach dem 1. bzw. 2. Fach (wie bei
#'   [merge_sc()]). Überschrift und Antwortoptionen werden aus `x1` genommen.
#' @param inkl1,inkl2 Wie `inkl` bei [merge_sc()], jeweils für eine der beiden
#'   Fragen; beim Standard `"nr1"` bzw. `"nr2"` entscheiden die Variablen
#'   `inkl.<nr1>` bzw. `inkl.<nr2>`. Ausgegeben wird nur, wenn beide Fragen
#'   eingeschlossen sind.
#' @param nr1,nr2 Fragenummern der beiden Fragen; beide erscheinen in der
#'   Überschrift.
#'
#' @returns Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
#'   [cat()] ausgegeben.
#'
#' @family auswertung
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' merge_subj(BspDaten$dataSHOWUP$fach1_2FB, BspDaten$dataSHOWUP$fach2_2FB)
#' \dontshow{
#' setwd(.old_wd)
#' }
#'
#' @export
merge_subj <- function(x1,
                       x2,
                       inkl1 = "nr1",
                       inkl2 = "nr2",
                       nr1 = "",
                       nr2 = "") {
  # Überprüfung der inkl-Parameter ----------------------------------------

  inkl1 <- .resolve_inkl(inkl1, nr1, marker = "nr1")

  inkl2 <- .resolve_inkl(inkl2, nr2, marker = "nr2")

  if (inkl1 != TRUE || inkl2 != TRUE) {
    return(invisible())
  } # wenn nicht beide inkl TRUE sind, wird Funktion beendet


  # Zusammenfügen der beiden Fächer-Spalten -------------------------------

  subj <- rbind(
    data.frame(fach = unlist(x1, use.names = FALSE)),
    data.frame(fach = unlist(x2, use.names = FALSE))
  )

  # Vergabe des neuen Labels ----------------------------------------------

  attr(subj$fach, "label") <- paste0(" & ", nr2, " ", sub(
    "\\?.*", "",
    attr(subj$fach, "label")
  ), " / 2. Fach? ")

  # Aufruf der merge_sc-Funktion ------------------------------------------

  merge_sc(subj$fach, nr = nr1)

  cat(paste(
    "*Hinweis: In der Befragung wurden 1. und 2. Fach getrennt abgefragt;",
    "in dieser Tabelle werden die Antworten gemeinsam dargestellt.",
    "Daraus ergibt sich in dieser Darstellung eine Verdopplung des",
    "Stichprobenumfangs (siehe „Total“).*  \n  \n"
  ))
}
