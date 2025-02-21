# Festlegen der Standardeinstellungen bei Laden des Pakets ----------------

set.analysis.defaults <- new.env(parent = emptyenv())

set.analysis.defaults$font.family <- "Red+Hat+Text"
set.analysis.defaults$col.width3 <- c(108, 18, 11)
set.analysis.defaults$col.width4 <- c(86, 18, 11, 18)
set.analysis.defaults$col.width.sm <- c(64, 11, 9, 9, 9, 9, 9)
set.analysis.defaults$col.width.sm.alt1 <- c(59, 8, 8, 8, 8, 8, 8, 9)
set.analysis.defaults$col.width.sm.alt2 <- c(54, 7, 6, 6, 6, 6, 6, 6, 9)
set.analysis.defaults$col1.width.tss <- 12
set.analysis.defaults$color.bars <- rgb(109, 172, 220, maxColorValue = 255)
set.analysis.defaults$color.font <- "steelblue"
set.analysis.defaults$table.color <- "tablecolor"
set.analysis.defaults$show.plot.sc <- TRUE
set.analysis.defaults$show.plot.mc <- TRUE
set.analysis.defaults$show.plot.sk <- TRUE
set.analysis.defaults$inkl.open <- TRUE

# Funktion, um diese Einstellungen zu ändern ------------------------------

#' Funktion um Einstellungsvariablen anzupassen
#'
#' @param ... Argumente, die die Einstellungen ändern sollen, z. B.
#' `color.bars = "red"` oder `show.plot.sc = FALSE`.
#' Die Namen der Argumente müssen mit den Namen der Einstellungsvariablen übereinstimmen.
#'
#' @export
#'
#' @examples
#' #Hier wird die Farbe der Balken auf rot geändert und eingestellt, dass keine SC-Plots gezeigt werden sollen
#' change.analysis.defaults(color.bars = "red", show.plot.sc = FALSE)
#' 
#' #Eine Überprüfung zeigt, dass die Änderungen erfolgreich waren
#' set.analysis.defaults$color.bars
#' set.analysis.defaults$show.plot.sc
#' 
#' #Diese Änderung geht nicht, weil die Variable nicht existiert
#' change.analysis.defaults(color.width2 = "turquoise")

change.analysis.defaults <- function(...) {
  changes <- list(...)
  
  # Überprüfen, ob die Einstellungsvariablen überhaupt existieren ---------
  for (i in seq_along(changes)) {
    if (!exists(names(changes)[i], envir = set.analysis.defaults)) {
      stop(paste0("Die Einstellungsvariable „", names(changes)[i], "“ existiert nicht."))
    }
  }
  
  # Einstellungen ändern --------------------------------------------------
  for (i in seq_along(changes)) {
      assign(names(changes)[i], changes[[i]], envir = set.analysis.defaults)
    }
}
