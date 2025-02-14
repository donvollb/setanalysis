# Festlegen der Standardeinstellungen bei Laden des Pakets ----------------

settings <- new.env(parent = emptyenv())

settings$font.family <- "Red+Hat+Text"
settings$col.width3 <- c(108, 18, 11)
settings$col.width4 <- c(86, 18, 11, 18)
settings$col.width.sm <- c(64, 11, 9, 9, 9, 9, 9)
settings$col.width.sm.alt1 <- c(59, 8, 8, 8, 8, 8, 8, 9)
settings$col.width.sm.alt2 <- c(54, 7, 6, 6, 6, 6, 6, 6, 9)
settings$col1.width.tss <- 12
settings$color.bars <- rgb(109, 172, 220, maxColorValue = 255)
settings$color.font <- "steelblue"
settings$table.color <- "tablecolor"
settings$show.plot.sc <- TRUE
settings$show.plot.mc <- TRUE
settings$show.plot.sk <- TRUE
settings$inkl.open <- TRUE

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
#' change_settings(color.bars = "red", show.plot.sc = FALSE)
#' 
#' #Eine Überprüfung zeigt, dass die Änderungen erfolgreich waren
#' settings$color.bars
#' settings$show.plot.sc
#' 
#' #Diese Änderung geht nicht, weil die Variable nicht existiert
#' change_settings(color.width2 = "turquoise")

change_settings <- function(...) {
  changes <- list(...)
  
  # Überprüfen, ob die Einstellungsvariablen überhaupt existieren ---------
  for (i in seq_along(changes)) {
    if (!exists(names(changes)[i], envir = settings)) {
      stop(paste0("Die Einstellungsvariable „", names(changes)[i], "“ existiert nicht."))
    }
  }
  
  # Einstellungen ändern --------------------------------------------------
  for (i in seq_along(changes)) {
      assign(names(changes)[i], changes[[i]], envir = settings)
    }
}
