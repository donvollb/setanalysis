#' Funktion für Fragen mit offenem Antwortformat (benötigt u.a. "list.open", wird am Anfang des Skripts erstellt)
#'
#' @param x Daten
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#' @param inkl.global Zweite inkl-Variable, die die globale Variable "inkl.open" abfragt. Kann auch in TRUE oder FALSE geändert werden
#' @param nr Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
#' @param freq Sollen gleiche offene Antworten zusammengefasst werden? Dann werden auch Häufigkeiten angezeigt
#' @param no.pagebreak Seitenumbrüche mittendrin verhindern?
#'
#' @examples open.answers(BspDaten$dataSHOWUP$offen) |> markdown_in_viewer()
#'
#' @export open.answers

open.answers <- function(x, # Daten
                         inkl = "nr",  # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                         inkl.global = set.analysis.defaults$inkl.open, # Zweite inkl-Variable, die die globale Variable "inkl.open" abfragt. Kann auch in TRUE oder FALSE geändert werden
                         nr = "", # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
                         freq = FALSE, # Sollen gleiche offene Antworten zusammengefasst werden? Dann werden auch Häufigkeiten angezeigt
                         no.pagebreak = TRUE) # Seitenumbrüche mittendrin verhindern?
{

  if (inkl == "nr") {
    if (nr == "") {inkl <- TRUE} else {inkl <- eval(parse(text = paste0("inkl.", nr)))}
  }

  if (inkl == TRUE && inkl.global == TRUE) {
    list.open.answers$anchor.nr <- list.open.answers$anchor.nr + 1
    anchor.nr <- list.open.answers$anchor.nr

    cat(paste0("### ", nr, " ", attr(x, "label"), " {#sec-", anchor.nr, ".top} \n\n"))

    if(length(na.omit(x)) > 0) {
      cat(paste0("*Die offenen Antworten zu dieser Frage finden sich im [Anhang](#sec-", anchor.nr, ".bottom).*  \n\n"))
    } else {
      cat("*Keine offenen Antworten zu dieser Frage.*  \n\n")
    }
    
    
    assign(paste0("var.", anchor.nr), x, envir = list.open.answers)
    assign(paste0("nr.", anchor.nr), nr, envir = list.open.answers)
  }
}