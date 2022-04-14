#################################################
########## INPUT BLANK UND TRUE FALSE ###########
#################################################

# ÄÖU richtig angezeigt? (Reopen with Encoding: UTF-8)

############################
# SETUP UND DATEN EINLESEN #
############################

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Arbeitsverzeichnis automatisch setzen
library(readxl)

# Blank-Datei

rstudioapi::showDialog("Blank einlesen", "Wähle die <b>blank-Datei</b> aus.")
file_blank <- file.choose()
blank <- read_excel(file_blank) # Einlesen der Blank-Datei
m <- ncol(blank) # wird für Schleife später benötigt, Anzahl der Spalten in der Blank Datei

# TRUE-FALSE Datei
rstudioapi::showDialog("TRUE-FALSE einlesen", "Wähle die <b>TRUE-FALSE-Datei</b> aus.")
file_rules <- file.choose()
rules <- read_excel(file_rules) # Einlesen der TRUE-FALSE Datei
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


  for ( i in 1:nrow(rules)) { #für jede inkl./Variable
    
    if (grepl(paste(names, collapse = "|"), rules$bed[i])) { #Falls einer der Spaltennamen vorkommt
      
      rules$bed_new[i] <- rules$bed[i] #Übertrage die Regel in die neue Spalte
      
      for (k in 1:length(names)) # Gehe alle Spaltennamen durch
      
        if (grepl(names[k], rules$bed[i])) { #Wenn ein Spaltenname vorkommt
        
      rules$bed_new[i] <- gsub(names[k], 
                               paste0("blank$", names[k]),
                               rules$bed_new[i])  # Setze vor den Namen ein "blank$" uns speichere es ab
        }
    }
    
    
    if (grepl("header", rules$var[i])){ #Wenn es um eine header-Variable geht
      
      nr <- as.numeric(sub("header", #Speichere die Nummer des headers ab (zum Beispiel header2 -> 2)
                "",
                rules$var[i]))
      l <- length(which(startsWith(rules$var, paste0("inkl.", nr, ".")))) # Wie viele inkl. Variablen gibt es zu diesem header?
      rules$bed_new[i] <- "" # Leere die Variable in bed-new (falls die Schleife aus Versehen mehrmals läuft)
      
      for (n in 1:l) { # Für jede inkl. Variable
        
      rules$bed_new[i] <- ifelse(rules$bed_new[i] == "", # Falls das Feld leer ist:
                                 paste0(rules$bed_new[i], "blank$inkl.", nr, ".", n, " == TRUE"), # Schreibe es ohne |
                                 paste0(rules$bed_new[i], " | blank$inkl.", nr, ".", n, " == TRUE")) # ansonsten mit | davor
      }
      
    }
 
    if (rules$bed[i] == "immer TRUE") {rules$bed_new[i] <- "immer TRUE"}  # Übertrage "immer TRUE", falls es in der Bedingung steht
    if (rules$bed[i] == "immer FALSE") {rules$bed_new[i] <- "immer FALSE"}    # Übertrage "immer FALSE", falls es in der Bedingung steht
    
    }
    
####################
# ENDE MAIN LOOP 1 #
####################



##########################################################################
# MAIN LOOP 2: Anwenden der Regeln und übertragen in den BLANK-Datensatz #
##########################################################################

# In dieser Schleife werden die Regeln der "rules"-Daten angewandt und in "blank" übertragen
# Dort wird dann automatisch für jeden Bericht entschieden, welche "inkl.-Variablen" auf "TRUE" gesetzt werden

for (n in 1:nrow(rules)) { #Für jede Regel
  
  f <- n + m #Addiere die Zeile plus die Anzahl der ursprünglichen Spalten in blank (damit die Variable dahinter gesetzt wird und nichts überschreibt)
  
  if (rules$bed_new[n] == "immer TRUE") { # Wenn die Regel "immer TRUE" ist
    blank[f] <- rep(TRUE, nrow(blank)) # Schreibe in allen Reihen "TRUE"
  } 
  
  
  
  if (rules$bed_new[n] == "immer FALSE") { # Wenn die Regel "immer FALSE" ist
    blank[f] <- rep(FALSE, nrow(blank)) # Schreibe in allen Reihen "FALSE"
  }
  
  
  
  if (rules$bed_new[n] != "immer FALSE" & rules$bed_new[n] != "immer TRUE") # Falls die Regel weder "immer TRUE" noch "immer FALSE" ist
    
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
blank[1, (m+1):ncol(blank)] <- TRUE # Setze TRUE in alle inkl-Spalten der ersten Zeile

###############
# ABSPEICHERN #
###############

rstudioapi::showDialog("Abspeichern", "Speichere die überarbeitete Blank-Datei ab. 
                       <b>Denke hierbei unbedingt an die Dateiendung (.csv)</b>")
# write.csv2(blank, file.choose(new = TRUE)) #Schreibe die Berichte Datei

# Falls man es manuell machen möchte:
# write.csv2(blank, "PFAD/DATEI.csv") #Schreibe die Berichte Datei

