# Erzeugt einen kleinen, fiktiven evasys-Export (Rohdaten + Codebuch) -----
#
# Die Struktur ist aus evasys_read_data() abgeleitet: Das Codebuch besteht aus
# Abschnitten pro Variable ("Variable:", "Fragetext:", "Fragetyp:", ggf.
# "Werte:" mit Zeilen "1: …"), getrennt durch eine Zeile "------"; nach dem
# letzten Abschnitt folgen zwei weitere Zeilen. Bei MC-Fragen steht der
# Variablenname mehrfach (Frage + eine Antwortoption pro Abschnitt).
# Beide Dateien sind wie bei evasys Latin-1-kodiert und durch Semikolon getrennt.
#
# Ausführen aus dem Paketverzeichnis:
# source("tests/testthat/fixtures/make_evasys_fixture.R")

rohdaten <- c(
  '"[FILTER] semester";"zufrieden";"abschluss_1";"abschluss_2";"kommentar";"alter"',
  '1;5;1;0;"Mehr Informationen zum Ablauf";23',
  '2;6;0;2;"";"-"',
  '1;2;1;2;"[Freitextfeld]";31',
  '2;4;0;0;".";27',
  '1;7;1;0;"Übersichtlicheres Modulhandbuch";"/"'
)

abschnitt <- function(variable, fragetext, fragetyp, werte = NULL) {
  c(
    paste0('Variable:;"', variable, '"'),
    paste0('Fragetext:;"', fragetext, '"'),
    paste0('Fragetyp:;"', fragetyp, '"'),
    if (!is.null(werte)) c("Werte:;", paste0(';"', werte, '"')),
    ";------"
  )
}

codebuch <- c(
  abschnitt(
    "semester", "1.1 In welchem Semester sind Sie?", "1 aus n",
    c("1: erstes Semester", "2: höheres Semester")
  ),
  abschnitt(
    "zufrieden", "1.2 Ich bin mit dem Studium zufrieden.", "Skalafrage",
    c(
      "1: trifft gar nicht zu", "2: ", "3: ", "4: ", "5: ", "6: trifft voll zu",
      "7: kann ich nicht beurteilen"
    )
  ),
  abschnitt("abschluss", "1.3 Welchen Abschluss streben Sie an? (Mehrfachnennung möglich)", "n aus m"),
  abschnitt("abschluss", "1.3 Welchen Abschluss streben Sie an? (Mehrfachnennung möglich) : Bachelor", "n aus m"),
  abschnitt("abschluss", "1.3 Welchen Abschluss streben Sie an? (Mehrfachnennung möglich) : Master", "n aus m"),
  abschnitt("kommentar", "2.1 Was möchten Sie uns noch mitteilen?", "Offene Frage"),
  abschnitt("alter", "2.2 Wie alt sind Sie?", "Offene Frage"),
  ";" # zweite Zeile nach dem letzten Abschnitt
)

ordner <- "tests/testthat/fixtures"
writeLines(iconv(rohdaten, "UTF-8", "latin1"), file.path(ordner, "evasys_rohdaten.csv"), useBytes = TRUE)
writeLines(iconv(codebuch, "UTF-8", "latin1"), file.path(ordner, "evasys_codebuch.csv"), useBytes = TRUE)
