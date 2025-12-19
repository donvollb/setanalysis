#' Funktion für offene Antworten
#' Die Funktion war ehemals auf zwei (jetzt veraltete) Funktionen aufgeteilt:
#' - `open.answers()`: Verweis auf Anhang bei Berichten mit Anhang
#' - `merge.open()`: Eigentliche Auswertung der offenen Antworten
#'
#' @param x Daten
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#' @param inkl_global Zweite inkl-Variable, die die globale Variable "inkl.open" abfragt. Kann auch in TRUE oder FALSE geändert werden
#' @param nr Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
#' @param freq Sollen gleiche offene Antworten zusammengefasst werden? Dann werden auch Häufigkeiten angezeigt.
#' "auto" führt zur Anzeige der Häufigkeiten, wenn Antworten mehrfach vorkommen, sonst nicht.
#' @param appendix Gibt es einen Extra-Anhang, in dem die offenen Antworten gesammelt werden sollen?
#' @param is_appendix Nur relevant, falls es einen Anhang gibt. Wenn TRUE, wird der Output für den Anhang erzeugt.
#' @param anchor Nur relevant, falls es einen Anhang gibt. Anker, damit auf den Output weiter oben im pdf Verlinkt werden kann.
#'
#' @examples
#'
#' # Beispiel für Bericht mit Anhang – Häufigkeiten werden angezeigt
#' {merge_open(BspDaten$dataSHOWUP$offen, appendix = TRUE) 
#' appendix.open()} |> markdown_in_viewer()
#' 
#' # Ergebnis für Bericht ohne Anhang – Ohne Häufigkeiten, weil jeder Eintrag nur einmal
#' merge_open(BspDaten$dataSHOWUP$offen, appendix = FALSE) |> markdown_in_viewer()
#'
#'
#'
#' @export merge_open
#' 

merge_open <- function(x, # Daten
                       inkl = "nr",  # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                       inkl_global = setanalysis_defaults$inkl.open, # Zweite inkl-Variable, die die globale Variable "inkl.open" abfragt. Kann auch in TRUE oder FALSE geändert werden
                       nr = "", # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
                       freq = "auto", # Sollen gleiche offene Antworten zusammengefasst werden? Dann werden auch Häufigkeiten angezeigt
                       appendix = setanalysis_defaults$open.apendix, # Gibt es einen Extra-Anhang, in dem die offenen Antworten gesammelt werden sollen?
                       is_appendix = FALSE, # Nur relevant, falls es einen Anhang gibt. Wenn TRUE, wird der Output für den Anhang erzeugt.
                       anchor = FALSE) # Nur relevant, falls es einen Anhang gibt. Wenn TRUE, wird der Output für den Anhang erzeugt.
{
  
  if (inkl == "nr") {
    if (nr == "") {inkl <- TRUE} else {inkl <- eval(parse(text = paste0("inkl.", nr)))}
  }
  
  if (inkl != TRUE | inkl_global != TRUE) {return(invisible())} # wenn nicht beide inkl-Arugmente TRUE sind, wird Funktion beendet
  
  ## Erzeugung des Outputs für den Hauptteil der Berichte, falls ----------
  ## es einen Extra Anhang für die offenen Antworten gibt -----------------
  
  if (appendix == TRUE & is_appendix == FALSE) {
    
    list.open.answers$anchor.nr <- list.open.answers$anchor.nr + 1
    anchor.nr <- list.open.answers$anchor.nr
    cat(paste0("### ", nr, " ", attr(x, "label"), " {#sec-", anchor.nr, ".top} \n\n"))

      if(length(na.omit(x)) > 0) {
        cat(paste0("*Die offenen Antworten zu dieser Frage finden sich im ",
                   "[Anhang](#sec-", anchor.nr, ".bottom).*  \n\n"))
  } else {
        cat("*Keine offenen Antworten zu dieser Frage.*  \n\n")
  }
   
    assign(paste0("var.", anchor.nr), x, envir = list.open.answers)
    assign(paste0("nr.", anchor.nr), nr, envir = list.open.answers)
    return(invisible())
  }
    
  ## Erzeugung des Eigentlichen Outputs mit den Offenen Fragen ------------
  
  if (appendix == FALSE | is_appendix == TRUE) {
    
    if (anchor != FALSE) {
      
    cat("###", nr, attr(x, "label"), paste0("{#sec-", anchor, ".bottom}"),  "\n \n")
    cat(paste0("[zurück nach oben](#sec-", anchor, ".top) \n\n"))
  } else {
    
    cat("###", nr, attr(x, "label"), "\n \n")
  }
    if(length(na.omit(x)) == 0) { # Falls es keine offenen Antworten gibt
    
    cat("*Keine offenen Antworten zu dieser Frage.*  \n\n")
    return(invisible())
  } 
  
  ### Leerzeichen vorne und hinten entfernen, NAs entfernen ---------------
    
  x <- trimws(x[!is.na(x)])
  
  ### Herausfinden, ob Häufigkeitstabelle sinnvoll ist (Gibt es Antworten mehrmals?)
  
  if(freq == "auto") {
    
    freq <- length(unique(tolower(x))) < length(x)
  }
  
  ### Alphabetisch sortieren und in Dataframe umwandeln -------------------
  
  x <- x[order(x)]
  x <- as.data.frame(x)
  
  ### Tabelle mit oder ohne Häufigkeiten erzeugen -------------------------
  
  if(freq == TRUE) {
    
    # Wieder in Vektor umwandeln
    x <- unlist(x, use.names = FALSE)
    
    # Gruppen nach Kleinbuchstaben bilden
    Gruppen <- split(x, tolower(x))
    
    # für jede Gruppe: die häufigste Schreibweise auswählen
    most_used <- function(x) {x |> table() |> which.max() |> names() |> first()}
    Hauptschreibweisen <- sapply(Gruppen, most_used)
    
    # Häufigkeiten (aller Varianten) zählen
    Häufigkeiten <- lengths(Gruppen)
    
    # Tabelle mit den Repräsentanten und den Häufigkeiten
    Tabelle <- data.frame(Antwort = Hauptschreibweisen,
                          Häufigkeit = Häufigkeiten,
                          row.names = NULL)
    
    # Nach Häufigkeit sortieren
    Tabelle <- Tabelle[order(-Tabelle$Häufigkeit, Tabelle$Antwort), ]
    
    # Formatierung der Tabelle
    subchunkify(lv.kable(Tabelle, col.width = c(137, 18),
                         striped = FALSE, escape = TRUE))
    
  } else {
    
    colnames(x) <- "Antwort"
    subchunkify(lv.kable(x, col.width = 159, striped = FALSE, escape = TRUE))
  }
  
  cat(" \n\n")

  }
}
                      
#' @noRd
#' @export

merge.open <- merge_open

#' @noRd
#' @export open.answers

open.answers <- function(...) merge.open(..., appendix = TRUE, is_appendix = FALSE)