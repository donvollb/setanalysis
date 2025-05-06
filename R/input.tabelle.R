#' Aufbereiten der Berichtstabelle
#'
#' Diese Funktion liest die Excel-Tabellen ein, in denen einmal die zu erstellenden Berichten und die TRUE-FALSE-REgeln stehen
#' 
#' @param blank.path Der Dateipfad zur rohen Berichtstabelle (blank-Tabelle) als Excel-Liste. Sofern nichts angegeben wird, kann man eine Datei über ein Dialogfenster auswählen.
#' @param rules.path Der Dateipfad zur zugehörigen Regeltabelle (TRUE-FALSE Tabelle) als Excel-Liste. Sofern nichts angegeben wird, kann man eine Datei über ein Dialogfenster auswählen.
#' @return Die aufbereitete Berichtstabelle als dataframe.
#' @examples
#' input.tabelle()
#' input.tabelle("path/to/blank/data.xlsx", "path/to/rules/data.xlsx")
#' @export

input.tabelle <- function(blank.path = NULL,
                          rules.path = NULL) {
  
  
  ############################
  # SETUP UND DATEN EINLESEN #
  ############################
  
  if (is.null(blank.path)) {
    rstudioapi::showDialog("Blank einlesen", "Wähle die <b>blank-Datei</b> aus.")
    blank.path <- file.choose()
  }
  blank <- readxl::read_excel(blank.path) # Einlesen der blank-Datei
  m <- ncol(blank) # wird für Schleife später benötigt, Anzahl der Spalten in der Blank Datei
  
  if (is.null(rules.path)) {
    rstudioapi::showDialog(
      "Regel-Tabelle einlesen",
      "Wähle die <b>Regel-Tabelle (TRUE-FALSE-Datei)</b> aus."
    )
    rules.path <- file.choose()
  }
  rules <- readxl::read_excel(rules.path) # Einlesen der TRUE-FALSE Datei
  colnames(rules) <- c("var", "bed", "anm") # Neue Spaltennamen
  
  # Weitere Vorbereitungen für Schleife
  rules$bed_new <- NA #Erstelle neue Spalte
  names <- colnames(blank) #Namen der Spalten von blank, wichtig für die Schleife
  names <- paste0(names, " ") # Leerzeichen hinter jeden Namen, damit z.B. Studiengang.Teil nicht miterkannt wird
  
  
  
  ###########################################
  # MAIN LOOP 1: Bearbeiten der Bedingungen #
  ###########################################
  
  # Diese Schleife bearbeitet die "Regeln" in der TRUE-FALSE-Datei und wandelt sie weiter in R-Code um
  # Aus "Studiengang == "Psychologie"" wird z.B. "blank$Studiengang == "Psychologie""
  # Zudem werden Regeln für header-Variablen erstellt (damit man das nicht manuell eintippen muss)
  
  
  for (i in 1:nrow(rules)) {
    #für jede inkl./Variable
    
    if (grepl(paste(names, collapse = "|"), rules$bed[i])) {
      #Falls einer der Spaltennamen vorkommt
      
      rules$bed_new[i] <- rules$bed[i] #Übertrage die Regel in die neue Spalte
      
      for (k in 1:length(names))
        # Gehe alle Spaltennamen durch
        
        if (grepl(names[k], rules$bed[i])) {
          #Wenn ein Spaltenname vorkommt
          
          rules$bed_new[i] <- gsub(names[k], paste0("blank$", names[k]), rules$bed_new[i])  # Setze vor den Namen ein "blank$" uns speichere es ab
        }
    }
    
    
    if (grepl("header", rules$var[i])) {
      #Wenn es um eine header-Variable geht
      
      nr <- as.numeric(sub("header", #Speichere die Nummer des headers ab (zum Beispiel header2 -> 2)
                           "", rules$var[i]))
      l <- length(which(startsWith(rules$var, paste0(
        "inkl.", nr, "."
      )))) # Wie viele inkl. Variablen gibt es zu diesem header?
      rules$bed_new[i] <- "" # Leere die Variable in bed-new (falls die Schleife aus Versehen mehrmals läuft)
      
      for (n in 1:l) {
        # Für jede inkl. Variable
        
        rules$bed_new[i] <- ifelse(
          rules$bed_new[i] == "",
          # Falls das Feld leer ist:
          paste0(rules$bed_new[i], "blank$inkl.", nr, ".", n, " == TRUE"),
          # Schreibe es ohne |
          paste0(rules$bed_new[i], " | blank$inkl.", nr, ".", n, " == TRUE")
        ) # ansonsten mit | davor
      }
      
    }
    
    if (rules$bed[i] == "immer TRUE") {
      rules$bed_new[i] <- "immer TRUE"
    }  # Übertrage "immer TRUE", falls es in der Bedingung steht
    if (rules$bed[i] == "immer FALSE") {
      rules$bed_new[i] <- "immer FALSE"
    }    # Übertrage "immer FALSE", falls es in der Bedingung steht
    
  }
  
  ####################
  # ENDE MAIN LOOP 1 #
  ####################
  
  
  
  ##########################################################################
  # MAIN LOOP 2: Anwenden der Regeln und übertragen in den BLANK-Datensatz #
  ##########################################################################
  
  # In dieser Schleife werden die Regeln der "rules"-Daten angewandt und in "blank" übertragen
  # Dort wird dann automatisch für jeden Bericht entschieden, welche "inkl.-Variablen" auf "TRUE" gesetzt werden
  
  for (n in 1:nrow(rules)) {
    #Für jede Regel
    
    f <- n + m #Addiere die Zeile plus die Anzahl der ursprünglichen Spalten in blank (damit die Variable dahinter gesetzt wird und nichts überschreibt)
    
    if (rules$bed_new[n] == "immer TRUE") {
      # Wenn die Regel "immer TRUE" ist
      blank[f] <- rep(TRUE, nrow(blank)) # Schreibe in allen Reihen "TRUE"
    }
    
    
    
    if (rules$bed_new[n] == "immer FALSE") {
      # Wenn die Regel "immer FALSE" ist
      blank[f] <- rep(FALSE, nrow(blank)) # Schreibe in allen Reihen "FALSE"
    }
    
    
    
    if (rules$bed_new[n] != "immer FALSE" &
        rules$bed_new[n] != "immer TRUE")
      # Falls die Regel weder "immer TRUE" noch "immer FALSE" ist
      
    {
      blank[f] <- ifelse(eval(parse(text = rules$bed_new[n])), TRUE, FALSE) #Interpretiere die Regel als "R-Code" und schreibe so die inkl. Variablen
      
    }
    
    colnames(blank)[f] <- rules$var[n] #Überschreibe den Namen der neuen Variable mit der jeweiligen inkl.Variable
  }
  
  ####################
  # ENDE MAIN LOOP 2 #
  ####################
  
  
  #################################################
  # ALLE INKL-VARIABLEN IM MASTER AUF TRUE SETZEN #
  #################################################
  
  # WICHTIG: Master-Bericht muss erster Bericht in blank sein!!!
  blank[1, (m + 1):ncol(blank)] <- TRUE # Setze TRUE in alle inkl-Spalten der ersten Zeile
  
  ###############
  # ABSPEICHERN #
  ###############
  
  return(blank)
  
}
