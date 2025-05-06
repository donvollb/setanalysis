#' Funktion für offene Antworten
#'
#' @param x Daten
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#' @param inkl.global Zweite inkl-Variable, die die globale Variable "inkl.open" abfragt. Kann auch in TRUE oder FALSE geändert werden
#' @param nr Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
#' @param anchor Falls über open.answers Anker kriiert wurden hier die Nummer angeben
#' @param freq Sollen gleiche offene Antworten zusammengefasst werden? Dann werden auch Häufigkeiten angezeigt
#'
#' @examples merge.open(BspDaten$dataSHOWUP$offen, anchor = 1) |> markdown.in.viewer()
#'
#' @export merge.open

merge.open <- function(x, # Daten
                       inkl = "nr",  # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                       inkl.global = set.analysis.defaults$inkl.open, # Zweite inkl-Variable, die die globale Variable "inkl.open" abfragt. Kann auch in TRUE oder FALSE geändert werden
                       nr = "", # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
                       anchor = FALSE, # Falls über open.answers Anker kreiert wurden hier die Nummer angeben
                       freq = FALSE) # Sollen gleiche offene Antworten zusammengefasst werden? Dann werden auch Häufigkeiten angezeigt
{


  if (inkl == "nr") {
    if (nr == "") {inkl <- TRUE} else {inkl <- eval(parse(text = paste0("inkl.", nr)))}
  }

  if (inkl == TRUE && inkl.global == TRUE) {

    if (anchor != FALSE)
    {
      cat("###", nr, attr(x, "label"), paste0("{#sec", anchor, ".bottom}"),  "\n \n")
      cat(paste0("[zurück nach oben](#sec-", anchor, ".top) \n\n"))
      
    } else {cat("###", nr, attr(x, "label"), "\n \n")}

    if(length(na.omit(x)) > 0) { # wenn mind. 1 offene Antwort

      x <- x[order(x)]
      x <- as.data.frame(x[!is.na(x)])


      if(freq == TRUE) {
        x <- data.frame(table(x))
        colnames(x) <- c("Antwort", "Häufigkeit")
        subchunkify(lv.kable(x, col.width = c(137, 18), striped = FALSE, escape = TRUE))


      } else {

        colnames(x) <- "Antwort"
        subchunkify(lv.kable(x, col.width = 159, striped = FALSE, escape = TRUE))
      }


    } else {

      cat("*Keine offenen Antworten zu dieser Frage.*\n")

    }

    cat("  \n  \n")

  }
}


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

anchor <- list.open.answers$anchor.nr

if (anchor == 1)  {return()} # stoppen, wenn keine offenen Fragen aufgerufen wurden
  
  cat("# Anhang: Fragen mit offenem Antwortformat  \n  \n")
  
  for (k in seq_along(anchor)) {
    x <- eval(parse(text = paste0("list.open.answers$var.", k)))
    q.nr <- eval(parse(text = paste0("list.open.answers$nr.", k)))
    merge.open(x, nr = q.nr, anchor = k)}
}
