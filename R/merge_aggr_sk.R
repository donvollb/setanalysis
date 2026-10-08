#' Skalenfragen gemeinsam auswerten, optional pro Lehrveranstaltung aggregiert
#'
#' @description
#' Erzeugt den Berichtsabschnitt für eine oder mehrere Skalenfragen mit
#' derselben Skala: eine gemeinsame Tabelle mit Kennwerten je Item (n, M, SD,
#' Median, Minimum, Maximum, optional Ausweichoptionen) und Boxplots je Item
#' ([boxplot_aggr_sk()]).
#'
#' Mit `aggr = TRUE` werden die Antworten zuerst je Lehrveranstaltung
#' (`kennung`) gemittelt. Jede Lehrveranstaltung geht dann mit ihrem
#' Mittelwert ein, und `n` zählt Lehrveranstaltungen statt Antworten.
#'
#' @param x Data Frame mit einer Spalte pro Item (oder ein einzelnes Item als
#'   Vektor). Jede Spalte braucht die Attribute `label` (Fragetext) und
#'   `labels` (benannte Antwortcodes).
#' @param kennung Vektor mit der Kennung der Lehrveranstaltung (oder einer
#'   anderen Gruppe) für jede Zeile von `x`. Nur bei `aggr = TRUE` nötig.
#' @param number Anzahl der Skalenstufen **ohne** Ausweichoptionen. Bei
#'   `"default"` die Anzahl der Antwortlabels der Items (alle Items müssen
#'   gleich viele haben). Haben Ausweichoptionen ein eigenes Label, sollte
#'   `number` angegeben werden.
#' @param alt1,alt2 Text der ersten bzw. zweiten Ausweichoption oder `FALSE`.
#'   Ist ein Text angegeben, zeigt die Tabelle eine zusätzliche Spalte mit der
#'   Häufigkeit dieser Antwort je Item. `alt2` ist nur zusammen mit `alt1`
#'   möglich.
#' @param alt1.num,alt2.num Code der ersten bzw. zweiten Ausweichoption in
#'   den Daten.
#' @param nr Fragenummer des **ersten** Items, z. B. `"2.1"`. Die folgenden
#'   Items werden fortlaufend nummeriert (`"2.2"`, `"2.3"`, …), die Nummern
#'   werden den Fragetexten vorangestellt. Bei `inkl = "nr"` wird jedes Item
#'   einzeln über seine Variable `inkl.<nr>` ein- oder ausgeschlossen.
#' @param tmin,tmid,tmax Beschriftung des linken Pols, der Mitte (nur bei
#'   ungerader Stufenzahl) und des rechten Pols. Bei `"default"` werden die
#'   Antwortlabels der Items verwendet; unterscheiden sie sich zwischen den
#'   Items, gibt es eine Warnung.
#' @param show.table Soll die Tabelle gezeigt werden?
#' @param show.plot Sollen die Boxplots gezeigt werden?
#' @param fig.height Höhe der Abbildung in Zoll. Bei `"default"` wird sie aus
#'   der Anzahl der Items berechnet.
#' @param col2.name Überschrift der Spalte mit der Anzahl (z. B. Anzahl der
#'   Lehrveranstaltungen bei `aggr = TRUE`).
#' @param message Text, der vor der Tabelle ausgegeben wird (Markdown), oder
#'   `""` für keinen.
#' @param aggr Sollen die Antworten zuerst je `kennung` gemittelt werden
#'   (siehe [aggr_data()])?
#' @inheritParams merge_sc
#'
#' @returns Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
#'   [cat()] ausgegeben.
#'
#' @family auswertung
#' @seealso [merge_sk()] für die Auswertung einer einzelnen Skalenfrage mit
#'   Häufigkeitsverteilung.
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' kernfragen <- BspDaten$dataLVE[, c("KF_01", "KF_02", "KF_03")]
#'
#' # Mittelwerte je Lehrveranstaltung
#' merge_aggr_sk(kernfragen, kennung = BspDaten$dataLVE$Kennung, aggr = TRUE)
#'
#' # Alle Antworten, nur Tabelle
#' merge_aggr_sk(kernfragen, show.plot = FALSE)
#'
#' if (interactive()) {
#'   markdown_in_viewer(
#'     merge_aggr_sk(kernfragen, kennung = BspDaten$dataLVE$Kennung, aggr = TRUE)
#'   )
#' }
#' \dontshow{
#' setwd(.old_wd)
#' }
#'
#' @export
merge_aggr_sk <- function(x,
                          kennung,
                          number = "default",
                          alt1 = FALSE,
                          alt2 = FALSE,
                          alt1.num = 0,
                          alt2.num = 7,
                          nr = "",
                          inkl = "nr",
                          tmin = "default",
                          tmid = "default",
                          tmax = "default",
                          show.table = TRUE,
                          show.plot = TRUE,
                          fig.height = "default",
                          col2.name = "n",
                          message = "",
                          aggr = FALSE) {
  if (inkl == "nr") {
    if (nr == "") {
      inkl <- TRUE
    } else {
      section <- sub("\\..*$", "", nr)
      first_item <- as.numeric(sub("^.*\\.", "", nr))
      last_item <- first_item + ncol(x) - 1
      item_numbers <- first_item:last_item
      item_nrs <- paste0(section, ".", item_numbers)

      # inkl.-Variable für jedes Item nachschlagen
      item_inkl <- sapply(item_nrs, .inkl_value, env = environment(), USE.NAMES = FALSE)

      x <- x[, item_inkl] # Variablen entfernen, die nicht vorkommen sollen
      item_nrs <- item_nrs[item_inkl] # Nummern entfernen, die nicht vorkommen sollen

      if (length(item_nrs) > 0) {
        for (k in seq_along(item_nrs)) {
          attr(x[, k], "label") <- paste(item_nrs[k], attr(x[, k], "label"))
        }
      }

      inkl <- any(item_inkl == TRUE)
    }
  }

  if (inkl != TRUE) {
    return(invisible())
  } # wenn inkl nicht TRUE, wird Funktion beendet

  if (number == "default") { # zieht sich automatisch die Anzahl der Stufen
    # Items, falls diese nicht angegeben wurde

    if (!is.null(ncol(x))) {
      n_levels <- unique(vapply(
        seq_len(ncol(x)), \(k) length(attr(x[, k], "labels")), integer(1)
      ))
      if (length(n_levels) != 1) { # Fehlermeldung bei unterschiedlicher Anzahl Stufen
        stop("Die ausgewählten Items haben eine unterschiedliche
           Anzahl an Stufen.")
      } else {
        number <- n_levels[[1]]
      }
    } else {
      number <- length(attr(x, "labels"))
    }
  }

  x <- data.frame(x)

  if (ncol(x) > 1) {
    labels <- as.character(lapply(x, attr, which = "label"))
  } else {
    labels <- attr(x[, 1], "label")
  }


  # Antwortlabels je Item (nicht relevante Labels werden abgeschnitten)
  level_labels <- lapply(seq_len(ncol(x)), \(k) names(attr(x[, k], "labels"))[1:number])

  level_label_table <- as.data.frame(level_labels, col.names = 1:ncol(x))

  if (tmin == "default") {
    labels_left <- unique(as.list(level_label_table[1, ]))

    if (length(unique(labels_left)) != 1) {
      warning(paste0("Achtung: Die Labels der einzelnen Items auf der linken
                   Seite sind unterschiedlich: \n", labels_left))
    }

    tmin <- labels_left[[1]]
  }

  if (tmid == "default" && number %% 2 == 1) { # nur bei ungerader Anzahl Stufen

    labels_mid <- unique(as.list(level_label_table[(number + 1) / 2, ]))

    if (length(unique(labels_mid)) != 1) {
      warning(paste0("Achtung: Die Labels der einzelnen Items in der Mitte
                      sind unterschiedlich:\n", labels_mid))
    }

    tmid <- labels_mid[[1]]
  }

  if (tmax == "default") {
    labels_right <- unique(as.list(level_label_table[number, ]))

    if (length(unique(labels_right)) != 1) {
      warning(paste0("Achtung: Die Labels der einzelnen Items auf der rechten
                    Seite sind unterschiedlich:\n", labels_right))
    }

    tmax <- labels_right[[1]]
  }


  if (aggr == TRUE) {
    x <- aggr_data(vars = x, kennung = kennung)
  }

  if (number %% 2 == 0 || tmid == "") { # bei gerader Anzahl Stufen oder keinem Mittellabel

    scale_text <- paste0(
      "(1)~", .escape_typst(tmin), " - (", number, ")~", .escape_typst(tmax)
    )
    scale_labels <- c(tmin, rep("", number - 2), tmax)
  } else { # bei ungerader Anzahl Stufen
    scale_text <- paste0(
      "(1)~", .escape_typst(tmin), " - (", (number + 1) / 2, ")~", .escape_typst(tmid),
      " - (", number, ")~", .escape_typst(tmax)
    )
    scale_labels <- c(
      tmin, rep("", (number - 3) / 2), tmid,
      rep("", (number - 3) / 2), tmax
    )
  }

  if (message != "") {
    cat(message)
  }


  # Häufigkeit der Ausweichoptionen je Item zählen (für die Tabelle) ------
  count_per_item <- function(value) {
    vapply(seq_len(ncol(x)), \(k) sum(x[, k] == value, na.rm = TRUE), integer(1))
  }

  if (alt1 != FALSE) {
    alt1.list <- count_per_item(alt1.num)
  }

  if (alt2 != FALSE) {
    alt2.list <- count_per_item(alt2.num)
  }

  x[x < 1 | x > number] <- NA

  if (show.table == TRUE) {
    subchunkify(
      table_stat_multi(
        x,
        col1.name = paste0('#text(weight: "bold")[Item] _[Skala: ', scale_text, "]_"),
        col2.name = col2.name,
        bold.corner = FALSE,
        alt1 = alt1,
        alt2 = alt2,
        alt1.list = alt1.list,
        alt2.list = alt2.list
      )
    )
  }

  if (show.plot == TRUE) {
    labels <- rev(labels)
    x <- rev(x)

    # Automatische Zeilenumbrüche einfügen --------------------------------

    labels <- .wrap_labels(labels, width = 49)

    # Höhe der Abbildung: eine Zeile pro Item plus Rand; bei ungerader Anzahl
    # Stufen etwas mehr Platz für den Hinweistext unter der Abbildung
    if (fig.height == "default") {
      fig.height <- length(labels) + if (number %% 2 != 0) 1.5 else 1
    }

    subchunkify(
      boxplot_aggr_sk(x, labels, scale_labels),
      fig_height = fig.height,
      fig_width = 9
    )
  }

  cat("  \n  \n")
}
