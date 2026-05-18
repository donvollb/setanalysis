# Code, der bei jedem Laden des Pakets ausgeführt wird --------------------

# Häufig verwendete Funktionen laden, dass man sie auch ohne „::“ nutzen kann

#' @importFrom grDevices rgb adjustcolor
#' @importFrom graphics abline axis barplot box boxplot mtext par segments text title
#' @importFrom stats median na.omit sd setNames
#' @importFrom utils capture.output read.csv2
#' @importFrom tinytable tt style_tt tt_format
#' @importFrom dplyr first
#' @importFrom svglite svglite
NULL

## Festlegen der Standardeinstellungen bei Laden des Pakets ---------------

#' Globale Umgebung für die Paketkonfiguration
#'
#' Diese Umgebung wird verwendet, um Konfigurationswerte wie Farben zu speichern.
#' @export setanalysis_defaults

setanalysis_defaults <<- new.env(parent = emptyenv())

setanalysis_defaults$font.family <- "Red Hat Text"
setanalysis_defaults$col.width3 <- c(108, 18, 11)
setanalysis_defaults$col.width4 <- c(86, 18, 11, 18)
setanalysis_defaults$col.width.sm <- c(64, 11, 9, 9, 9, 9, 9)
setanalysis_defaults$col.width.sm.alt1 <- c(59, 8, 8, 8, 8, 8, 8, 15)
setanalysis_defaults$col.width.sm.alt2 <- c(52, 7, 7, 7, 5, 6, 6, 12, 12)
setanalysis_defaults$col1.width.tss <- 12
setanalysis_defaults$color.bars <- rgb(109, 172, 220, maxColorValue = 255)
setanalysis_defaults$show.plot.sc <- TRUE
setanalysis_defaults$show.plot.mc <- TRUE
setanalysis_defaults$show.plot.sk <- TRUE
setanalysis_defaults$open.appendix <- TRUE
setanalysis_defaults$inkl.open <- TRUE

#' Umgebung für die offenen Antworten -------------------------------------
#'
#' Diese Umgebung wird verwendet, um die offenen Antworten für die verschiedenen Fragen zu speichern.
#' @export list.open.answers

list.open.answers <<- new.env(parent = emptyenv())

list.open.answers$anchor.nr <- 0


## Bei Start des Pakets Schriftart laden ----------------------------------

.onAttach <- function(libname, pkgname) {
  
 try(silent = TRUE, { # Fehlermeldungen ignorieren (ist für Installation nötig)
  
  if (!"Red Hat Text" %in% sysfonts::font_families()) {
   
  sysfonts::font_add("Red Hat Text", 
    regular = system.file("fonts/RHMixed-Regular.ttf", package = "setanalysis"),
       bold = system.file("fonts/RHMixed-Bold.ttf",    package = "setanalysis"),
     italic = system.file("fonts/RHMixed-Light.ttf",   package = "setanalysis"))
  
  showtext::showtext_auto()
  }}
)
}

# Funktion, um diese Einstellungen zu ändern ------------------------------

#' Funktion um Einstellungsvariablen anzupassen
#'
#' @param ... Argumente, die die Einstellungen ändern sollen, z. B.
#' `color.bars = "red"` oder `show.plot.sc = FALSE`.
#' Die Namen der Argumente müssen mit den Namen der Einstellungsvariablen übereinstimmen.
#'
#' @export change.analysis.defaults
#'
#' @examples
#' 
#' # Hier wird die Farbe der Balken auf rot geändert und eingestellt,
#' # dass keine SC-Plots gezeigt werden sollen
#' 
#' change.analysis.defaults(color.bars = "red", show.plot.sc = FALSE)
#' 
#' #Eine Überprüfung zeigt, dass die Änderungen erfolgreich waren
#' 
#' setanalysis_defaults$color.bars
#' setanalysis_defaults$show.plot.sc
#' 
#' #Diese Änderung ginge nicht, weil die Variable nicht existiert
#' #change.analysis.defaults(color.width2 = "turquoise")
#' 
change.analysis.defaults <- function(...) {
  changes <- list(...)
  
  # Überprüfen, ob die Einstellungsvariablen überhaupt existieren ---------
  for (i in seq_along(changes)) {
    if (!exists(names(changes)[i], envir = setanalysis_defaults)) {
      stop(paste0("Die Einstellungsvariable „", names(changes)[i], "“ existiert nicht."))
    }
  }
  
  # Einstellungen ändern --------------------------------------------------
  for (i in seq_along(changes)) {
      assign(names(changes)[i], changes[[i]], envir = setanalysis_defaults)
    }
}