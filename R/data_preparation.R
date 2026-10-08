# Aufbereitung und Prüfung von Daten -------------------------------------

#' Testet, ob die Labels aus personalized.info so im Datensatz vorkommen
#'
#' @param vars Variablen (oder eine Variable), die aggregiert werden sollen
#' @param kennung Die Kennungen (z. B. LV-Kennungen oder Fallnummern), nach denen die Daten aggregiert werden sollen
#'
#' @export aggr_data

# Funktion zum Aggregieren von Daten anhand einer Kennung/Fallnummer
aggr_data <- function(vars, # Variablen (oder eine Variable), die aggregiert werden sollen
                      kennung) # kennung können z.B. die LV-Kennungen oder die Fallnummern sein
{
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

#' Testet, ob die Labels aus personalized.info so im Datensatz vorkommen
#'
#' @param col Spalte aus der Info-Tabelle, z.B. info$Fb.text
#' @param var Variable aus Datensatz, die der Spalte entspricht
#' @param exception Ausnahmen, die nicht überprüft werden sollen
#'
#' @examples
#' # In diesem Fall wird die Variable "FB.text" aus der Info mit der Variable
#' # "Teilbereich" aus dem Datensatz verglichen und alles stimmt
#' label_test(BspDaten$pInfo$FB.txt, BspDaten$dataLVE$Teilbereich)
#'
#' # So sieht es aus, wenn die Labels nicht komplett übereinstimmen:
#' label_test(BspDaten$pInfo$FB.txt.falsch, BspDaten$dataLVE$Teilbereich)
#'
#' @export label_test

# Testen, ob Labels aus personalized.info so im Datensatz vorkommen
label_test <- function(col, # Spalte aus der Info-Tabelle, z.B. info$Fb.text
                       var, # Variable aus Datensatz, die der Spalte entspricht
                       exception = "alle") { # Ausnahmen, die nicht überprüft werden sollen

  labels_col <- unique(col)

  if (length(exception != 0)) {
    for (k in seq_along(exception)) {
      labels_col <- labels_col[labels_col != exception[k]]
    }
  }

  if (!is.null(attr(var, "levels"))) {
    labels_var <- attr(var, "levels")
  } else {
    labels_var <- unique(var)
  }

  if (all(labels_col %in% labels_var)) {
    output <- "Alle Labels der Spalte aus personalized.info kommen in gleicher Schreibweise auch in der Variable vor"
  } else {
    false_labels <- labels_col[which(!(labels_col %in% labels_var))]
    output <- paste0("Das Label \"", false_labels, "\" aus der Spalte von personalized.info kommt nicht in gleicher Schreibweise in den Labels der Variable vor.")
  }
  return(print(output))
}
