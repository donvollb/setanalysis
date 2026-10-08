# Erzeugt die fiktiven Beispieltabellen für input.tabelle() ---------------
#
# Die Tabellen sind echten Berichtstabellen nachempfunden, enthalten aber nur
# erfundene Studiengänge. Sie werden in den Beispielen der Dokumentation und
# in den Tests verwendet.
#
# Ausführen aus dem Paketverzeichnis: source("data-raw/beispieltabellen.R")
# Benötigt das Paket writexl (nur zum Erzeugen, nicht zur Nutzung des Pakets).

# Berichtstabelle ("blank"): eine Zeile pro Bericht -----------------------
# Wichtig: Der Master-Bericht (alle Fragen) muss in der ersten Zeile stehen.

berichte <- data.frame(
  Code = c(
    "MASTER", "Gesamtbericht", "B.Sc._Musterwissenschaft",
    "M.Sc._Musterwissenschaft", "B.A._Beispielkunde", "Studienberatung"
  ),
  Art = c(
    "alles.master", "alles", "Studiengang", "Studiengang", "Studiengang",
    "speziell"
  ),
  Abschluss = c(
    "alle", "alle", "Bachelor of Science (B.Sc.)",
    "Master of Science (M.Sc.)", "Bachelor of Arts (B.A.)", "alle"
  ),
  Studiengang = c(
    "alle", "alle", "Musterwissenschaft", "Musterwissenschaft",
    "Beispielkunde", "alle"
  ),
  Titel = c(
    "Befragung 2024: Master", "Befragung 2024: Gesamtbericht",
    "Befragung 2024: B.Sc. Musterwissenschaft",
    "Befragung 2024: M.Sc. Musterwissenschaft",
    "Befragung 2024: B.A. Beispielkunde",
    "Befragung 2024: Studienberatung"
  )
)

# Regeltabelle: wann kommt welche Frage in einen Bericht? -----------------
# Spalte 1: Name der inkl.-Variable (inkl.<Abschnitt>.<Frage>) oder header<Abschnitt>
# Spalte 2: Bedingung mit den Spaltennamen der Berichtstabelle (R-Syntax),
#           "immer TRUE" oder "immer FALSE". Für header-Zeilen wird die
#           Bedingung automatisch erzeugt (der Text dort ist nur ein Hinweis).

regeln <- data.frame(
  Variable = c(
    "inkl.1.1", "inkl.1.2", "inkl.1.3",
    "inkl.2.1", "inkl.2.2",
    "inkl.3.1",
    "header1", "header2", "header3"
  ),
  Bedingung = c(
    "immer TRUE",
    'Art != "speziell"',
    'Abschluss == "Bachelor of Science (B.Sc.)" | Art == "alles"',
    'Studiengang == "Musterwissenschaft"',
    "immer FALSE",
    'Art == "speziell"',
    "eine der inkl.1.x-Variablen == TRUE",
    "eine der inkl.2.x-Variablen == TRUE",
    "eine der inkl.3.x-Variablen == TRUE"
  )
)
names(regeln)[2] <- "TRUE (in einen Bericht rein), wenn…"

writexl::write_xlsx(berichte, "inst/extdata/beispiel_berichte.xlsx")
writexl::write_xlsx(regeln, "inst/extdata/beispiel_regeln.xlsx")
