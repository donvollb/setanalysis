#' Funktion für Abbildungen analog zu alten EvaSys-Skalen
#'
#' @param x Daten
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#' @param nr Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
#' @param show.alt Zeige Ausweichoptionen, falls es sie gibt
#' @param number Anzahl Antwortoptionen des Items (OHNE AUSWEICHOPTIONEN!)
#' @param alt1 Text für erste Ausweichoption (standardmäßig 0 in den Daten, siehe alt1.num)
#' @param alt2 Text für zweite Ausweichoption (standardmäßig 7 in den Daten, siehe alt1.num)
#' @param alt1.num Welche Zahl entspricht alt1
#' @param alt2.num Welche Zahl entspricht alt2
#' @param lime Handelt es sich um exportierte LimeSurvey-Daten?
#' @param lime.brackets Müssen eckige Klammern um den Fragetext herum entfernt werden?
#' @param show.plot Zeige Plot?
#' @param no.pagebreak Sollen Seitenumbrüche verhindert werden?
#'
#' @export

merge.evasys.sk <- function(x, # Daten
                            inkl = "nr", # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                            nr = "", # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
                            show.alt = TRUE, # Zeige Ausweichoptionen, falls es sie gibt
                            number = 6, # Skala (OHNE AUSWEICHOPTIONEN!)
                            alt1 = FALSE, # Text für erste Ausweichoption (standardmäßig 0 in den Daten, siehe alt1.num)
                            alt2 = FALSE, # Text für zweite Ausweichoption (standardmäßig 7 in den Daten, siehe alt1.num)
                            alt1.num = 0, # Welche Zahl entspricht alt1
                            alt2.num = 7, # Welche Zahl entspricht alt2
                            lime = FALSE, # Für Daten im Format nach LimeSurvey Export (nach Syntax-Skript)
                            lime.brackets = FALSE, # Müssen eckige Klammern um den Fragetext herum entfernt werden?
                            show.plot = set.analysis.defaults$show.plot.sk, # Zeige Plot?
                            no.pagebreak = TRUE) # Seitenumbrüche verhindern?
{

  if (sum(!is.na(x)) > 0) {

    if (inkl == "nr") {
      if (nr == "") {inkl <- TRUE} else {inkl <- eval(parse(text = paste0("inkl.", nr)))}
    }

    if (inkl == TRUE) {

      if (lime == TRUE) {

        temp <- attr(x, "label")

        levs <- levels(x)
        x <- as.numeric(x, na.rm = TRUE)

        for (l in 1:length(levs)) {
          x <- sjlabelled::add_labels(x, labels = setNames(l, levs[l]))
        }

        if(lime.brackets == TRUE) {
          temp <- sub("^\\[", "", temp)
          temp <- sub("].*$", "", temp)
        }

        attr(x, "label") <- temp

      }
      if(no.pagebreak == TRUE) {cat("\\begin{minipage}{\\linewidth} \n")}
      cat("\\subsubsection{ " , nr, " ", replace.latex.issues(attr(x, "label")), "}  \n  \n")
      cat("  \n  \n")

      xtab <- x
      xtab <- xtab[xtab %in% c(1:number)]
      print(table.stat.single(xtab, col1.name = "n", md = TRUE))

      cat("  \n  \n")

      if (show.alt == TRUE) {
        if(alt1 != FALSE) {

          cat("\\begin{center}Die Ausweichoption \"\\textit{", alt1, "}\" wurde ", sum(x == alt1.num, na.rm = TRUE),
              " mal gewählt.\\end{center}  \n  \n", sep = "")

        }
        if(alt2 != FALSE) {cat("\\begin{center}Die Ausweichoption \"\\textit{", alt2, "}\" wurde ", sum(x == alt2.num, na.rm = TRUE),
                               " mal gewählt.\\end{center}  \n  \n", sep = "")
        }

        labels <- names(attributes(x)$labels)
        tmin <- labels[1]
        tmax <- labels[number]

        cat("  \n \n")

        if(show.plot == TRUE) {
          subchunkify(evasys.skala.plot (x, tmin, tmax, number = number), fig_height = 1.4, fig_width = 6)
        }

      }
      if(no.pagebreak == TRUE) {
        cat("\n\\bigskip")
        cat("\n\\end{minipage}")
      }
      cat("   \n  \n")
    }

  }
}
