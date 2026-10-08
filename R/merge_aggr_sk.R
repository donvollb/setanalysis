#' merge-Funktion für die Darstellung der aggregierten Ergebnisse einer oder
#' mehrerer Skalenfragen#
#'
#' `merge.multi.sk()` ist eine veraltete Schreibweise der gleichen Funktion

#'
#' @param x Itemdaten (Dataframe mit einer oder mehreren Spalten)
#' @param kennung Objekt mit Kennungen (oder Fallnummern, nur bei Aggregierung benötigt)
#' @param number Anzahl Antwortoptionen der Items (OHNE AUSWEICHOPTIONEN!), wird bei "default" automatisch gezogen
#' @param alt1 Text für erste Ausweichoption (standardmäßig 0 in den Daten, siehe alt1.num)
#' @param alt2 Text für zweite Ausweichoption (standardmäßig 7 in den Daten, siehe alt1.num)
#' @param alt1.num Welche Zahl entspricht alt1
#' @param alt2.num Welche Zahl entspricht alt2
#' @param nr Nummer der ersten Frage
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#' @param tmin linker Pol, bei "default" wird das Label automatisch gezogen
#' @param tmid mittlerer Pol (für 5er Skalen), bei "default" automatisch
#' @param tmax rechter Pol, "default" wie oben
#' @param show.table Soll die Tabelle angezeigt werden?
#' @param show.plot Sollen Boxplots dazu angezeigt werden?
#' @param fig.height Höhe der Abbildung, bei "default" ist es Anzahl der Fragen + 1
#' @param col2.name Titel der n-Spalte
#' @param message Soll ein Hinweistext am Anfang erfolgen?
#' @param aggr Sollen Daten aggregiert werden?
#'
#' @examples
#' # Objekt erstellen, dass mehrere Fragen enthält:
#' KF_123 <- BspDaten$dataLVE[, c("KF_01", "KF_02", "KF_03")]
#'
#' # Funktion ausführen:
#' markdown_in_viewer(merge_aggr_sk(KF_123,
#'   number = 6, aggr = TRUE,
#'   kennung = BspDaten$dataLVE$Kennung
#' ))
#'
#' @export merge_aggr_sk


merge_aggr_sk <- function(x, # Daten
                          kennung, # Objekt mit Kennungen (oder Fallnummern, nur bei Aggregierung benötigt
                          number = "default", # Skala: 6 für Sechser, etc. (OHNE AUSWEICHOPTION)
                          alt1 = FALSE, # Text für erste Ausweichoption (standardmäßig 0 in den Daten, siehe alt1.num)
                          alt2 = FALSE, # Text für zweite Ausweichoption (standardmäßig 7 in den Daten, siehe alt1.num)
                          alt1.num = 0, # Welche Zahl entspricht alt1
                          alt2.num = 7, # Welche Zahl entspricht alt2
                          nr = "", # Nummer der ersten Frage
                          inkl = "nr", # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                          tmin = "default", # linker Pol, bei "default" wird das Label automatisch gezogen
                          tmid = "default", # mittlerer Pol (für 5er Skalen), bei "default" automatisch
                          tmax = "default", # rechter Pol, "default" wie oben
                          show.table = TRUE, # Soll Tabelle angezeigt werden?
                          show.plot = TRUE, # Sollen Boxplots dazu angezeigt werden?
                          fig.height = "default", # Höhe der Abbildung, bei "default" ist es Anzahl der Fragen + 1
                          col2.name = "n", # Titel der n-Spalte, in LVE in "N\\textsubscript{courses}" ändern
                          message = "", # Soll ein Hinweistext am Anfang erfolgen?
                          aggr = FALSE) # Sollen Daten aggregiert werden?
{
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

      inkl <- ifelse(any(item_inkl == TRUE), TRUE, FALSE)
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

  if (tmid == "default" & number %% 2 == 1) { # nur bei ungerader Anzahl Stufen

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

  if (number %% 2 == 0 | tmid == "") { # bei gerader Anzahl Stufen oder keinem Mittellabel

    scale_text <- paste0("(1)~", tmin, " - (", number, ")~", tmax)
    scale_labels <- c(tmin, rep("", number - 2), tmax)
  } else { # bei ungerader Anzahl Stufen
    scale_text <- paste0(
      "(1)~", tmin, " - (", (number + 1) / 2, ")~", tmid,
      " - (", number, ")~", tmax
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
