########################################
########## DATENAUFBEREITUNG ###########
########################################

# ÄÖU richtig angezeigt? (Reopen with Encoding: UTF-8)


##########################
# SETUP + DATEN EINLESEN # 
##########################

# Arbeitsverzeichnis
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Arbeitsverzeichnis automatisch setzen


# Datein einlesen
# Direkt den csv-Export von EvaSys einlesen
rstudioapi::showDialog("Daten einlesen", "Wähle die <b>Rohdaten</b> (csv-Export von Evasys) aus.")
file_data <- file.choose()
data <- read.csv2(file_data, fileEncoding = "latin1")

# Codebuch einlesen (aus Evasys); "header = FALSE", damit nicht erste Zeile als Spaltennamen genutzt werden 
# Hier das Codebuch einlesen
rstudioapi::showDialog("Codebuch einlesen", "Wähle das <b>Codebuch</b> (csv-Export von Evasys) aus.")
file_codebook <- file.choose()
codebook <- read.csv2(file_codebook, header = FALSE, col.names = c("var", "code"), fileEncoding = "latin1")

# Anführungsstriche im Codebuch entfernen
codebook[, 2] <- gsub("\"", "", codebook[, 2])
codebook[, 2] <- gsub("'", "", codebook[, 2])

# Alle Variablennamen aus dem Codebuch extrahieren
var.names.raw <- unique(codebook[codebook$var == "Variable:", 2])




###################
### MAIN LOOP 1 ###
###################

# Multiple-Choice-Frage bestehen im Datensatz aus mehreren Variablen (eine pro Antwortoption)
# Im Datensatz sind diese mit Nummern unterschiedlich benannt (z.B. Variablenname_1, Variablenname_2, Variablenname_3, etc.)
# Im Codebuch heißen alle Variablennamen gleich, daher müssen wir diese noch durchnummerieren, damit es zum Datensatz passt


# Bilde eine Schleife mit allen Variablennamen
for (i in 1:length(var.names.raw)) {
  
  var.name.tmp <-
    var.names.raw[i]  # Speichere den Variablennamen temporär ab
  
  
  # Wenn ein Variablenname nicht direkt im Datensatz vorkommt (das ist bei den MC-Fragen dann ja der Fall)
  if (!(var.name.tmp %in% colnames(data))) {
    
    vec.tmp <-
      which(codebook[, 2] == var.name.tmp) # Prüfe nach, in welchen Zeilen der Variablenname im Codebuch steht
    
    
    # Wichtig: Im Codebuch gibt es für jede MC-Frage "1+Anzahl Antwortoptionen"-Abschnitte, der erste muss nicht geändert werden, der Rest wird durchnummeriert
    
    
    # Bilde eine Schleife mit allen Positionen, wo der Variablenname steht (es geht bei "2" los, da der erste Abschnitt ja nicht geändert werden muss)
    for (n in 2:length(vec.tmp)) {
      
      
      codebook[vec.tmp[n], 2] <-
        paste0(codebook[vec.tmp[n], 2], "_", n - 1) # Schreibe hinten die Nummer an den Variablennamen
    }
  }
}


# Speichere nun erneut alle Variablennamen aus dem Codebuch (jetzt wurden ja einige hinten nummeriert)
var.names <- unique(codebook[codebook$var == "Variable:", 2])

###################
### MAIN LOOP 2 ###
###################

# In der nächsten Schleife ziehen wir dann die wichtigen Infos (Labels, etc.) aus dem Codebuch und schreiben sie mit in den Datensatz

# Bilde eine Schleife mit allen Variablennamen
for (i in 1:length(var.names)) {
  
  var.name.tmp <- var.names[i] # Speichere den aktuellen Variablennamen ab
  
  if (var.name.tmp %in% colnames(data)) { # Wenn der Variablenname im Datensatz als Spalte vorkommt
    
    # Hier wird es ein wenig tricky: Die einzelnen Abschnitte im Codebuch sind durch "------" getrennt, dann kommt die nächste Variable
    # Wir wollen nun den Abschnitt aus dem Codebuch extrahieren, in em die Infos zur aktuellen Variable stehen
    # Dafür starten wir in der Zeile, in der der Variablenname steht
    # Dann suchen wir uns noch die Zeile, in der der Variablenname der nächsten Variable steht und ziehen davon 2 ab (dann landen wir in der letzten Zeile der vorherigen Variable)
    # Damit haben wir die Start- und Endzeile des relevanten Abschnitts
    # Wenn wir uns bei der letzten Variable befinden, geht das ja nicht mehr, da nehmen wir dann einfach die drittletzte Zeile als Ende
    
    # Falls es sich nicht um die letzte Variable handelt
    if (i != length(var.names)) {
      attrs <- codebook[which(codebook$code == var.name.tmp):(which(codebook$code == var.names[i+1])[1]-2), ] # Extrahiere die relevanten Zeilen aus dem Codebuch
    } else {
        attrs <- codebook[which(codebook$code == var.name.tmp):(nrow(codebook)-3), ] # Falls es die letzte ist, nimm als Ende die drittletzte Zeile
      }
      
    # Nun haben wir im Objekt attrs den Abschnitt im Codebuch gespeichert, der die Informationen zu aktuelle Variable enthält
    # Daraus ziehen wir jetzt folgende Infos:
    
      attr(data[, var.name.tmp], "label") <- sub("^.*? ", "", attrs[attrs$var == "Fragetext:", 2]) # Den Fragetext als "label"
      attr(data[, var.name.tmp], "nr") <- sub("? .*$", "", attrs[attrs$var == "Fragetext:", 2]) # Die Nummer der Frage im Fragebogen als "nr"
      attr(data[, var.name.tmp], "type") <- attrs[attrs$var == "Fragetyp:", 2] # Den Fragetyp als "type"
      
      # In den nächsten Zeilen passen wir die Typbezeichnung ein wenig an, aus "1 aus n" wird z.B. "sc"
      if (attr(data[, var.name.tmp], "type") == "1 aus n") {attr(data[, var.name.tmp], "type") <- "sc"}
      if (attr(data[, var.name.tmp], "type") == "n aus m") {attr(data[, var.name.tmp], "type") <- "mc"}
      if (attr(data[, var.name.tmp], "type") == "Skalafrage") {attr(data[, var.name.tmp], "type") <- "sk"}
      if (attr(data[, var.name.tmp], "type") == "Offene Frage") {attr(data[, var.name.tmp], "type") <- "open/num"}
      
      
      # Nun fehlen nur noch die Value Labels, also was z.B. die Antwortoption "1" bei der Frage nach dem Abschluss bedeutet
      
      # Da dass nur bei Skalen- oder SC-Fragen nötig ist, prüfen wir mit einer if-Klausel, ob es sich um eine solche Frage handelt
      if(attrs[attrs$var == "Fragetyp:", 2] %in% c("1 aus n", "Skalafrage")) {
        
        # Die beiden folgenden Objekte legen wir leer an, wir brauchen die später
        nums <- NULL # Die Nummern der Antwortoptionen (z.B. 1 bis 6)
        nams <- NULL # Was die Nummern dann bedeuten (z.B. "trifft gar nicht zu")
    
        # Die Antortoptionen stehen über mehrere Zeilen verteilt im "attrs"-Objekt, daher läuft unsere Schleife über jede der Zeilen
        for (k in (which(attrs$var %in% c("Wert:", "Werte:"))+1):nrow(attrs)) {
          nums <- c(nums, as.numeric(sub(": .*?$", "", attrs[k, 2]))) # Hier schnappt sie sich die Nummer
          nams <- c(nams, sub("^.*?: ", "", attrs[k, 2])) # Hier die Antwortoption
          # Diese werden jeweils zu den Objekten hinzugefügt
        }
    
        # Hier werden dann die Antwortoptionen als "labels" der variable hinzugefügt
        attr(data[, var.name.tmp], "labels") <- setNames(nums, nams) 
    }
  }
}

###################
### FEINSCHLIFF ###
###################

# Jeweils bestimmte Antworten durch NA ersetzen (wirkt sich nur auf die offenen Fragen aus)
data[data == ""] <- NA
data[data == "-"] <- NA
data[data == "."] <- NA
data[data == ". "] <- NA
data[data == "/"] <- NA
data[data == "[Freitextfeld]"] <- NA # durch die csv in die Daten gelangt


# Optional: Falls Jahreszahlen korrigiert werden müssen (z.B. offene Antwort in Zahlen nach Geburtsjahr, manche schreiben 1996, manche nur 96)

# Bei diesem Beispiel war es bei der ShowUp2122 die Frage 1.20
# tmp <- as.numeric(data[, 32])
# tmp[tmp < 22 & !is.na(tmp)] <- tmp[tmp < 21 & !is.na(tmp)] + 2000
# tmp[tmp > 22 & tmp < 100 & !is.na(tmp)] <- tmp[tmp < 21 & tmp < 100 & !is.na(tmp)] + 1900
# data[1:nrow(data), 32] <- tmp
# rm(tmp)



###############
# ABSPEICHERN #
###############
# Abspeichern, unbedingt hinten an die Datei ".rda" schreiben!!!!!
rstudioapi::showDialog("Abspeichern", "Speichere die aufbereiteten Daten ab ab. 
                       <b>Denke hierbei unbedingt an die Dateiendung (.rda)</b>")
save(data, file = file.choose(new = TRUE))

# Falls man es manuell eingeben möchte:
# save(data, file = "PFAD/NAME.rda")

