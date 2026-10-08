#' Aufbereitung von Rohdaten aus evasys
#'
#' Diese Funktion liest Rohdaten aus EvaSys ein und bereitet sie auf. Dafür wird der Rohdatensatz als csv-Datei sowie das zugehörige Codebuch als csv-Datei benötigt.
#'
#' @param raw.data.path Der Dateipfad zu den Rohdaten (eine csv-Datei). Sofern nichts angegeben wird, kann man eine Datei über ein Dialogfenster auswählen.
#' @param codebook.path Der Dateipfad zum zugehörigen Codebook aus evasys (eine csv-Datei). Sofern nichts angegeben wird, kann man eine Datei über ein Dialogfenster auswählen.
#' @return Der aufbereitete Datensatz als dataframe.

#' @export

evasys_read_data <- function(raw.data.path = NULL, codebook.path = NULL) {
  # Rohdaten und Codebuch einlesen --------------------------------------------

  # Direkt den csv-Export von EvaSys einlesen
  if (is.null(raw.data.path)) {
    rstudioapi::showDialog("Daten einlesen", "Wähle die <b>Rohdaten</b> (csv-Export von Evasys) aus.")
    raw.data.path <- file.choose()
  }

  data <- read.csv2(raw.data.path, fileEncoding = "latin1")

  if (is.null(codebook.path)) {
    rstudioapi::showDialog("Codebuch einlesen", "Wähle das <b>Codebuch</b> (csv-Export von Evasys) aus.")
    codebook.path <- file.choose()
  }

  # Codebuch einlesen (aus Evasys); "header = FALSE", damit nicht erste Zeile als Spaltennamen genutzt werden
  # Hier das Codebuch einlesen
  codebook <- read.csv2(codebook.path, header = FALSE, col.names = c("var", "code"), fileEncoding = "latin1")

  # Anführungsstriche im Codebuch entfernen
  codebook[, 2] <- gsub("\"", "", codebook[, 2])
  codebook[, 2] <- gsub("'", "", codebook[, 2])

  # Alle Variablennamen aus dem Codebuch extrahieren
  var_names_raw <- unique(codebook[codebook$var == "Variable:", 2])

  # Einige Variablennamen bekommen bei Evasys wegen der Filter "X.Filter.." am Anfang, das muss korrigiert werden
  colnames(data) <- gsub("^X\\.FILTER\\.\\.", "", colnames(data))


  # Schleife 1: MC-Variablen im Codebuch durchnummerieren ---------------------

  # Multiple-Choice-Frage bestehen im Datensatz aus mehreren Variablen (eine pro Antwortoption)
  # Im Datensatz sind diese mit Nummern unterschiedlich benannt (z.B. Variablenname_1, Variablenname_2, Variablenname_3, etc.)
  # Im Codebuch heißen alle Variablennamen gleich, daher müssen wir diese noch durchnummerieren, damit es zum Datensatz passt


  # Bilde eine Schleife mit allen Variablennamen
  for (i in seq_along(var_names_raw)) {
    var_name <-
      var_names_raw[i] # Speichere den Variablennamen temporär ab


    # Wenn ein Variablenname nicht direkt im Datensatz vorkommt (das ist bei den MC-Fragen dann ja der Fall)
    if (!(var_name %in% colnames(data))) {
      positions <-
        which(codebook[, 2] == var_name) # Prüfe nach, in welchen Zeilen der Variablenname im Codebuch steht


      # Wichtig: Im Codebuch gibt es für jede MC-Frage "1+Anzahl Antwortoptionen"-Abschnitte, der erste muss nicht geändert werden, der Rest wird durchnummeriert


      # Bilde eine Schleife mit allen Positionen, wo der Variablenname steht (es geht bei "2" los, da der erste Abschnitt ja nicht geändert werden muss)
      for (n in 2:length(positions)) {
        codebook[positions[n], 2] <-
          paste0(codebook[positions[n], 2], "_", n - 1) # Schreibe hinten die Nummer an den Variablennamen
      }
    }
  }


  # Speichere nun erneut alle Variablennamen aus dem Codebuch (jetzt wurden ja einige hinten nummeriert)
  var_names <- unique(codebook[codebook$var == "Variable:", 2])

  # Schleife 2: Label, Nummer, Typ und Value Labels übernehmen ----------------

  # In der nächsten Schleife ziehen wir dann die wichtigen Infos (Labels, etc.) aus dem Codebuch und schreiben sie mit in den Datensatz

  # Bilde eine Schleife mit allen Variablennamen
  for (i in seq_along(var_names)) {
    var_name <- var_names[i] # Speichere den aktuellen Variablennamen ab

    if (var_name %in% colnames(data)) { # Wenn der Variablenname im Datensatz als Spalte vorkommt

      # Hier wird es ein wenig tricky: Die einzelnen Abschnitte im Codebuch sind durch "------" getrennt, dann kommt die nächste Variable
      # Wir wollen nun den Abschnitt aus dem Codebuch extrahieren, in em die Infos zur aktuellen Variable stehen
      # Dafür starten wir in der Zeile, in der der Variablenname steht
      # Dann suchen wir uns noch die Zeile, in der der Variablenname der nächsten Variable steht und ziehen davon 2 ab (dann landen wir in der letzten Zeile der vorherigen Variable)
      # Damit haben wir die Start- und Endzeile des relevanten Abschnitts
      # Wenn wir uns bei der letzten Variable befinden, geht das ja nicht mehr, da nehmen wir dann einfach die drittletzte Zeile als Ende

      # Falls es sich nicht um die letzte Variable handelt
      if (i != length(var_names)) {
        section <- codebook[which(codebook$code == var_name):(which(codebook$code == var_names[i + 1])[1] - 2), ] # Extrahiere die relevanten Zeilen aus dem Codebuch
      } else {
        section <- codebook[which(codebook$code == var_name):(nrow(codebook) - 2), ] # Falls es die letzte ist, nimm als Ende die vorletzte Zeile
      }

      # Nun haben wir im Objekt section den Abschnitt im Codebuch gespeichert, der die Informationen zu aktuelle Variable enthält
      # Daraus ziehen wir jetzt folgende Infos:

      attr(data[, var_name], "label") <- sub("^.*? ", "", section[section$var == "Fragetext:", 2]) # Den Fragetext als "label"
      attr(data[, var_name], "nr") <- sub(" .*$", "", section[section$var == "Fragetext:", 2]) # Die Nummer der Frage im Fragebogen als "nr"
      # Den Fragetyp als "type", dabei die evasys-Bezeichnung in die Kurzform
      # des Pakets übersetzen (aus "1 aus n" wird z.B. "sc")
      evasys_type <- section[section$var == "Fragetyp:", 2]
      type_names <- c(
        "1 aus n" = "sc", "n aus m" = "mc", "Skalafrage" = "sk", "Offene Frage" = "open/num"
      )
      attr(data[, var_name], "type") <-
        if (evasys_type %in% names(type_names)) type_names[[evasys_type]] else evasys_type


      # Nun fehlen nur noch die Value Labels, also was z.B. die Antwortoption "1" bei der Frage nach dem Abschluss bedeutet

      # Da dass nur bei Skalen- oder SC-Fragen nötig ist, prüfen wir mit einer if-Klausel, ob es sich um eine solche Frage handelt
      if (section[section$var == "Fragetyp:", 2] %in% c("1 aus n", "Skalafrage")) {
        # Die Antwortoptionen stehen in den Zeilen nach "Wert:" bzw. "Werte:",
        # jeweils in der Form "1: trifft gar nicht zu"
        value_rows <- section[(which(section$var %in% c("Wert:", "Werte:")) + 1):nrow(section), 2]
        values <- as.numeric(sub(": .*?$", "", value_rows)) # Die Nummern der Antwortoptionen (z.B. 1 bis 6)
        value_labels <- sub("^.*?: ", "", value_rows) # Was die Nummern bedeuten (z.B. "trifft gar nicht zu")

        # Hier werden dann die Antwortoptionen als "labels" der variable hinzugefügt
        attr(data[, var_name], "labels") <- setNames(values, value_labels)
      }
    }
  }

  # Platzhalter in offenen Antworten durch NA ersetzen ------------------------

  # Jeweils bestimmte Antworten durch NA ersetzen (wirkt sich nur auf die offenen Fragen aus)
  data[data == ""] <- NA
  data[data == "-"] <- NA
  data[data == "."] <- NA
  data[data == ". "] <- NA
  data[data == "/"] <- NA
  data[data == "[Freitextfeld]"] <- NA # durch die csv in die Daten gelangt


  return(data)
}
