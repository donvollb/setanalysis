# Erzeugt einen kleinen, fiktiven evasys-Export (Rohdaten + Codebuch) -----
#
# Das Codebuch ist im Format eines echten evasys-Codebuchs aufgebaut (Inhalte
# erfunden): Abschnitte pro Variable mit "Variable:", "Fragetyp:",
# "Fragetext:", ggf. "Zeichenlimit:" und "Wert:"/"Werte:"; die erste Zeile
# nach "Werte:" mit Antwortoptionen beginnt in der Folgezeile ("1 : …").
# Abschnitte sind durch "------------" getrennt (zwischen Fragengruppen
# doppelt), Texte stehen in dreifachen Anführungszeichen. Bei MC-Fragen steht
# der Variablenname mehrfach (Frage + eine Antwortoption pro Abschnitt).
# Beide Dateien sind wie bei evasys Latin-1-kodiert und durch Semikolon getrennt.
#
# Ausführen aus dem Paketverzeichnis:
# source("data-raw/beispiel_evasys.R")

rohdaten <- c(
  '"[FILTER] semester";"zufrieden";"abschluss_1";"abschluss_2";"kommentar";"alter"',
  '1;5;1;0;"Mehr Informationen zum Ablauf";23',
  '2;6;0;2;"";"-"',
  '1;2;1;2;"[Freitextfeld]";31',
  '2;4;0;0;".";27',
  '1;7;1;0;"Übersichtlicheres Modulhandbuch";"/"'
)

trenner <- "------------"
optionen <- function(werte) paste0('" ";"""', werte, '"""')

codebuch <- c(
  'Variable:;"""semester"""',
  'Fragetyp:;"1 aus n"',
  'Fragetext:;"""1.1 In welchem Semester sind Sie?"""',
  'Wert:;"<leer>: Ungültig / Keine Antwort"',
  optionen(c("1: erstes Semester", "2: höheres Semester")),
  trenner,
  'Variable:;"""zufrieden"""',
  "Fragetyp:;Skalafrage",
  'Fragetext:;"""1.2 Ich bin mit dem Studium zufrieden."""',
  'Werte:;"<leer>: Ungültig / Keine Antwort"',
  optionen(c(
    "1 : trifft gar nicht zu", "2 : ", "3 : ", "4 : ", "5 : ", "6 : trifft voll zu",
    "7 : kann ich nicht beurteilen"
  )),
  trenner,
  'Variable:;"""abschluss"""',
  'Fragetyp:;"n aus m"',
  'Fragetext:;"""1.3 Welchen Abschluss streben Sie an? (Mehrfachnennung möglich)"""',
  trenner,
  'Variable:;"""abschluss"""',
  'Fragetyp:;"n aus m"',
  paste0(
    'Fragetext:;"""\'1.3 Welchen Abschluss streben Sie an? ',
    '(Mehrfachnennung möglich)\' : Bachelor"""'
  ),
  'Werte:;"0 : nicht angekreuzt"',
  '" ";"""1"": angekreuzt"',
  trenner,
  'Variable:;"""abschluss"""',
  'Fragetyp:;"n aus m"',
  paste0(
    'Fragetext:;"""\'1.3 Welchen Abschluss streben Sie an? ',
    '(Mehrfachnennung möglich)\' : Master"""'
  ),
  'Werte:;"0 : nicht angekreuzt"',
  '" ";"""2"": angekreuzt"',
  trenner,
  trenner,
  'Variable:;"""kommentar"""',
  "Zeichenlimit:;1500",
  'Fragetyp:;"Offene Frage"',
  'Fragetext:;"""2.1 Was möchten Sie uns noch mitteilen?"""',
  'Wert:;"Antworttext / Platzhalter"',
  trenner,
  'Variable:;"""alter"""',
  "Zeichenlimit:;3",
  'Fragetyp:;"Offene Frage"',
  'Fragetext:;"""2.2 Wie alt sind Sie?"""',
  'Wert:;"Antworttext / Platzhalter"',
  trenner
)

ordner <- "inst/extdata"
writeLines(iconv(rohdaten, "UTF-8", "latin1"), file.path(ordner, "beispiel_evasys_rohdaten.csv"), useBytes = TRUE)
writeLines(iconv(codebuch, "UTF-8", "latin1"), file.path(ordner, "beispiel_evasys_codebuch.csv"), useBytes = TRUE)
