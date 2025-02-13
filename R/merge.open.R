#' Funktion für offene Antworten
#'
#' @param x Daten
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#' @param inkl.global Zweite inkl-Variable, die die globale Variable "inkl.open" abfragt. Kann auch in TRUE oder FALSE geändert werden
#' @param nr Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
#' @param anchor Falls über open.answers Anker kriiert wurden hier die Nummer angeben
#' @param freq Sollen gleiche offene Antworten zusammengefasst werden? Dann werden auch Häufigkeiten angezeigt
#'
#' @export

merge.open <- function(x, # Daten
                       inkl = "nr",  # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                       inkl.global = inkl.open, # Zweite inkl-Variable, die die globale Variable "inkl.open" abfragt. Kann auch in TRUE oder FALSE geändert werden
                       nr = "", # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
                       anchor = FALSE, # Falls über open.answers Anker kriiert wurden hier die Nummer angeben
                       freq = FALSE) # Sollen gleiche offene Antworten zusammengefasst werden? Dann werden auch Häufigkeiten angezeigt
{


  if (inkl == "nr") {
    if (nr == "") {inkl <- TRUE} else {inkl <- eval(parse(text = paste0("inkl.", nr)))}
  }

  if (inkl == TRUE && inkl.global == TRUE) {

    if (anchor != FALSE)
    {
      cat(paste0("\\hypertarget{", anchor, ".bottom}{}\\subsubsection{" , nr, " ", replace.latex.issues(attr(x, "label")), "}  \n  \n"))
      cat(paste0("\\hyperlink{", anchor, ".top}{zurück nach oben}  \n  \n"))
    } else {cat("\\subsubsection{ " , nr, " ", replace.latex.issues(attr(x, "label")), "}  \n  \n")}

    if(length(na.omit(x)) > 0) { # wenn mind. 1 offene Antwort

      x <- x[order(x)]
      x <- replace.latex.issues(x, all = FALSE)
      x <- as.data.frame(x[!is.na(x)])


      if(freq == TRUE) {
        x <- data.frame(table(x))
        colnames(x) <- c("Antwort", "Häufigkeit")
        #        print(lv.kable(x, col.width = c("388pt", "50pt"), striped = FALSE, escape = TRUE))
        print(lv.kable(x, col.width = c(137, 18), striped = FALSE, escape = TRUE))


      } else {

        colnames(x) <- "Antwort"
        #        print(lv.kable(x, col.width = "450pt", striped = FALSE, escape = TRUE))
        print(lv.kable(x, col.width = 159, striped = FALSE, escape = TRUE))
      }


    } else {

      cat("\\textit{Keine offenen Antworten zu dieser Frage.}  \n")

    }

    cat("  \n  \n")

  }
}
