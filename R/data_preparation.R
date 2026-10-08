# Aufbereitung und Prüfung von Daten -------------------------------------

#' Antworten je Gruppe mitteln
#'
#' @description
#' Berechnet für jede Gruppe (z. B. jede Lehrveranstaltung) den Mittelwert
#' jeder Variable. So geht später jede Gruppe gleich stark in Tabellen und
#' Abbildungen ein, unabhängig davon, wie viele Antworten sie hat. Wird von
#' [merge_aggr_sk()] und [merge_grade()] verwendet.
#'
#' @param vars Data Frame mit den zu mittelnden Variablen oder ein einzelner
#'   Vektor. Werte werden in Zahlen umgewandelt, fehlende Werte ignoriert.
#' @param kennung Vektor mit der Gruppenzugehörigkeit (z. B. Kennung der
#'   Lehrveranstaltung) für jede Zeile von `vars`.
#'
#' @returns Data Frame mit einer Zeile pro Gruppe (in der Reihenfolge, in der
#'   die Gruppen in `kennung` zuerst vorkommen) und einer Spalte pro Variable.
#'   Die Attribute `label` der Variablen bleiben erhalten.
#'
#' @family daten
#'
#' @examples
#' lve <- BspDaten$dataLVE
#' mittelwerte <- aggr_data(lve[, c("KF_01", "KF_02")], lve$Kennung)
#' head(mittelwerte)
#'
#' @export
aggr_data <- function(vars, kennung) {
  labels <- as.character(lapply(data.frame(vars), attr, which = "label"))
  x <- data.frame(data.frame(vars)[0, ])
  for (n in unique(kennung)) {
    group_vars <- data.frame(data.frame(vars)[kennung == n, ])
    x[nrow(x) + 1, ] <- group_vars |>
      apply(2, as.numeric) |>
      apply(2, mean, na.rm = TRUE)
  }

  for (k in seq_len(ncol(x))) {
    attr(x[, k], "label") <- labels[k]
  }

  return(x)
}

#' Schreibweisen in Berichtstabelle und Daten abgleichen
#'
#' @description
#' Prüft, ob alle Einträge einer Spalte der Berichtstabelle (z. B. die Namen
#' der Fachbereiche, nach denen die Daten je Bericht gefiltert werden) in
#' genau dieser Schreibweise auch in den Daten vorkommen. So fallen
#' Tippfehler auf, bevor ein Bericht versehentlich leer bleibt.
#'
#' @param col Spalte der Berichtstabelle, z. B. `info$FB.txt`.
#' @param var Passende Variable im Datensatz. Bei einem Faktor werden seine
#'   Stufen verwendet, sonst die vorkommenden Werte.
#' @param exception Einträge von `col`, die nicht geprüft werden (Standard:
#'   `"alle"`, z. B. für einen Gesamtbericht).
#'
#' @returns Die Meldung als Text (unsichtbar); sie wird außerdem ausgegeben.
#'   Bei Abweichungen gibt es eine Meldung pro nicht gefundenem Eintrag.
#'
#' @family daten
#'
#' @examples
#' # Alles stimmt
#' label_test(BspDaten$pInfo$FB.txt, BspDaten$dataLVE$Teilbereich)
#'
#' # Ein Eintrag mit Tippfehler
#' label_test(BspDaten$pInfo$FB.txt.falsch, BspDaten$dataLVE$Teilbereich)
#'
#' @export
label_test <- function(col, var, exception = "alle") {
  labels_col <- unique(col)

  # Ausnahmen nicht prüfen
  labels_col <- labels_col[!labels_col %in% exception]

  if (!is.null(attr(var, "levels"))) {
    labels_var <- attr(var, "levels")
  } else {
    labels_var <- unique(var)
  }

  if (all(labels_col %in% labels_var)) {
    output <- "Alle Einträge der Spalte aus der Berichtstabelle kommen in gleicher Schreibweise auch in der Variable vor."
  } else {
    false_labels <- labels_col[which(!(labels_col %in% labels_var))]
    output <- paste0("Der Eintrag \"", false_labels, "\" aus der Berichtstabelle kommt nicht in gleicher Schreibweise in der Variable vor.")
  }
  return(print(output))
}
