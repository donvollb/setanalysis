#' Festlegen, welche Fragen in welchen Bericht kommen
#'
#' @description
#' Liest zwei Excel-Tabellen ein und berechnet für jeden Bericht, welche
#' Fragen er enthält:
#'
#' * die **Berichtstabelle** („blank“): eine Zeile pro Bericht mit
#'   Merkmalen wie Studiengang oder Abschluss. Die **erste Zeile muss der
#'   Master-Bericht** sein, der alle Fragen enthält.
#' * die **Regeltabelle**: eine Zeile pro Frage bzw. Abschnitt. Die erste
#'   Spalte enthält den Namen (`inkl.<Abschnitt>.<Frage>`, z. B. `inkl.2.1`,
#'   oder `header<Abschnitt>`), die zweite die Bedingung.
#'
#' Bedingungen sind R-Code mit den Spaltennamen der Berichtstabelle, z. B.
#' `Art != "speziell"` oder `Abschluss == "Bachelor of Science (B.Sc.)"`.
#' Zusätzlich gibt es `immer TRUE` und `immer FALSE`. Für `header`-Zeilen wird
#' die Bedingung automatisch erzeugt: Der Abschnitt erscheint, sobald eine
#' seiner Fragen erscheint (der Text in der Tabelle wird ignoriert). Die
#' `header`-Zeilen müssen deshalb nach den zugehörigen `inkl`-Zeilen stehen.
#'
#' Im Berichts-Template werden die Werte einer Zeile als Variablen
#' (`inkl.2.1` usw.) gesetzt; die Auswertungsfunktionen fragen sie über ihr
#' Argument `nr` ab (siehe [merge_sc()]).
#'
#' @param blank.path Pfad zur Excel-Datei mit der Berichtstabelle. Ohne
#'   Angabe öffnet sich ein Dialog zur Dateiauswahl (nur in RStudio).
#' @param rules.path Pfad zur Excel-Datei mit der Regeltabelle. Ohne Angabe
#'   öffnet sich ein Dialog zur Dateiauswahl (nur in RStudio).
#'
#' @returns Die Berichtstabelle (Tibble) mit einer zusätzlichen logischen
#'   Spalte pro Regel. In der ersten Zeile (Master-Bericht) sind alle diese
#'   Spalten `TRUE`.
#'
#' @family daten
#'
#' @examples
#' # Fiktive Beispieltabellen mit sechs Berichten
#' berichte <- system.file("extdata", "beispiel_berichte.xlsx", package = "setanalysis")
#' regeln <- system.file("extdata", "beispiel_regeln.xlsx", package = "setanalysis")
#'
#' tabelle <- input_tabelle(berichte, regeln)
#' tabelle[, c("Code", "inkl.1.2", "inkl.2.1", "header2", "inkl.3.1")]
#'
#' @export
input_tabelle <- function(blank.path = NULL,
                          rules.path = NULL) {
  # Dateien einlesen (ohne Pfad: Auswahl per Dialogfenster) ---------------

  if (is.null(blank.path)) {
    rstudioapi::showDialog("Blank einlesen", "Wähle die <b>blank-Datei</b> aus.")
    blank.path <- file.choose()
  }
  blank <- readxl::read_excel(blank.path) # eine Zeile pro Bericht
  n_original <- ncol(blank)

  if (is.null(rules.path)) {
    rstudioapi::showDialog(
      "Regel-Tabelle einlesen",
      "Wähle die <b>Regel-Tabelle (TRUE-FALSE-Datei)</b> aus."
    )
    rules.path <- file.choose()
  }
  rules <- readxl::read_excel(rules.path)
  rule_vars <- rules[[1]] # Name der Variable, z. B. "inkl.2.1" oder "header2"
  conditions <- rules[[2]] # Bedingung, z. B. 'Art != "speziell"'

  # Regeln nacheinander auswerten -----------------------------------------
  # Jede Regel ergibt eine neue Spalte (TRUE/FALSE pro Bericht), die hinten
  # angehängt wird. Die Reihenfolge ist wichtig: header-Regeln greifen auf
  # die zuvor erzeugten inkl.-Spalten zurück.

  for (i in seq_along(rule_vars)) {
    value <- .evaluate_rule(
      rule_vars[i], conditions[i],
      rule_vars = rule_vars, data = blank, env = parent.frame()
    )
    blank[n_original + i] <- ifelse(value, TRUE, FALSE)
    colnames(blank)[n_original + i] <- rule_vars[i]
  }

  # Im Master-Bericht alle Fragen einschließen ----------------------------
  # WICHTIG: Der Master-Bericht muss in der ersten Zeile der blank-Datei stehen!

  blank[1, (n_original + 1):ncol(blank)] <- TRUE

  blank
}

# Eine Regel der Regeltabelle für alle Berichte auswerten
#
# - "immer TRUE" / "immer FALSE": für alle Berichte gleich
# - header-Zeilen (z. B. "header2"): TRUE, wenn eine der Variablen
#   inkl.2.1, inkl.2.2, … TRUE ist (die Bedingung in der Tabelle wird ignoriert)
# - sonst: Die Bedingung ist R-Code und wird mit den Spalten der
#   Berichtstabelle ausgewertet, z. B. 'Art != "speziell"'
.evaluate_rule <- function(rule_var, condition, rule_vars, data, env) {
  if (condition == "immer TRUE") {
    return(rep(TRUE, nrow(data)))
  }
  if (condition == "immer FALSE") {
    return(rep(FALSE, nrow(data)))
  }

  if (grepl("header", rule_var)) {
    section_nr <- as.numeric(sub("header", "", rule_var))
    n_items <- sum(startsWith(rule_vars, paste0("inkl.", section_nr, ".")))
    item_cols <- paste0("inkl.", section_nr, ".", 1:n_items)
    return(Reduce(`|`, lapply(item_cols, \(col) data[[col]] == TRUE)))
  }

  eval(str2lang(condition), envir = data, enclos = env)
}
