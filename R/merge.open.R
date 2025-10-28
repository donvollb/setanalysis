#' Funktion für offene Antworten
#'
#' @param x Daten
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#' @param inkl.global Zweite inkl-Variable, die die globale Variable "inkl.open" abfragt. Kann auch in TRUE oder FALSE geändert werden
#' @param nr Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
#' @param anchor Falls über open.answers Anker kriiert wurden hier die Nummer angeben
#' @param freq Sollen gleiche offene Antworten zusammengefasst werden? Dann werden auch Häufigkeiten angezeigt
#'
#' @examples merge.open(BspDaten$dataSHOWUP$offen, anchor = 1) |> markdown_in_viewer()
#'
#' @export merge.open

merge.open <- function(x, # Daten
                       inkl = "nr",  # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                       inkl.global = set.analysis.defaults$inkl.open, # Zweite inkl-Variable, die die globale Variable "inkl.open" abfragt. Kann auch in TRUE oder FALSE geändert werden
                       nr = "", # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
                       anchor = FALSE, # Falls über open.answers Anker kreiert wurden hier die Nummer angeben
                       freq = "auto") # Sollen gleiche offene Antworten zusammengefasst werden? Dann werden auch Häufigkeiten angezeigt
{

  if (inkl == "nr") {
    if (nr == "") {inkl <- TRUE} else {inkl <- eval(parse(text = paste0("inkl.", nr)))}
  }

  if (inkl != TRUE | inkl.global != TRUE) {return(invisible())} # wenn inkl nicht TRUE, wird Funktion beendet

  if (anchor != FALSE)
  {
    cat("###", nr, attr(x, "label"), paste0("{#sec-", anchor, ".bottom}"),  "\n \n")
    cat(paste0("[zurück nach oben](#sec-", anchor, ".top) \n\n"))
    
  } else {cat("###", nr, attr(x, "label"), "\n \n")}

  if(length(na.omit(x)) == 0) { # Falls es keine offenen Antworten gibt
    
    cat("*Keine offenen Antworten zu dieser Frage.*\n\n\n")
    return(invisible())
  } 

  # Leerzeichen vorne und hinten entfernen, NAs entfernen
  x <- trimws(x[!is.na(x)])
  
  # Herausfinden, ob Häufigkeitstabelle sinnvoll ist (Gibt es Antworten mehrmals?)
  if(freq == "auto") {
    
    freq <- length(unique(tolower(x))) < length(x)
  }
  
  # Alphabetisch sortieren und in Dataframe umwandeln
  x <- x[order(x)]
  x <- as.data.frame(x)


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

