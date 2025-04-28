#' merge-Funktion für single-choice Fragen
#'
#' @param x Daten
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#' @param nr Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
#' @param fig.height Höhe der Abbildung, bei "default" ist es „Anzahl der Fragen“ * 0.75 + 1
#' @param already.labels Wurden die Daten bereits in Label umgewandelt?
#' @param col2.name Name der n-Spalte in der Tabelle
#' @param order.table Soll nach Häufigkeit sortiert werden? "decreasing" für absteigendes Sortieren
#' @param show.plot Soll der Plot angezeigt werden?
#' @param no.pagebreak Seitenumbrüche mittendrin verhindern?
#'
#' @examples markdown.in.viewer(merge.sc(BspDaten$dataLVE$V3_D, inkl = TRUE, nr = 1))
#'
#' @export merge.sc

merge.sc <- function(x, # Daten
                     inkl = "nr", # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                     nr = "", # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
                     fig.height = "default", # Höhe der Abbildung, bei "default" ist es Anzahl der Fragen*0.75 +1
                     already.labels = FALSE, # Wurden die Daten bereits in Label umgewandelt?
                     col2.name = "n", # Name der n-Spalte in Tabelle
                     order.table = FALSE, # Soll nach Häufigkeit sortiert werden? "decreasing" für absteigendes Sortieren
                     show.plot = set.analysis.defaults$show.plot.sc, # Soll der Plot angezeigt werden?
                     no.pagebreak = TRUE) # Seitenumbrüche mittendrin verhindern?
{
  if (sum(!is.na(x)) > 0) {

    if(already.labels == FALSE) {x <- sjlabelled::to_label(x)}

    if (inkl == "nr") {
      if (nr == "") {inkl <- TRUE} else {inkl <- eval(parse(text = paste0("inkl.", nr)))}
    }

    if (inkl == TRUE) {
      if(no.pagebreak == TRUE) #{cat("\\begin{minipage}{\\linewidth} \n")} # funktioniert nicht in Quarto
      {cat("{{< pagebreak >}} \n  \n")}
      cat("###", nr, attr(x, "label"), "\n \n")



      #      print(table.freq(x, col1.name = "Antwortoption", col2.name = col2.name, # kein Print bei Flextable
      #                       order.table = order.table))
      flextable::flextable_to_rmd(table.freq(x, col1.name = "Antwortoption", col2.name = col2.name,
                                  order.table = order.table))

      freq.tab <- descr::freq(x, plot = FALSE)
      results <- data.frame(rownames(freq.tab), round(freq.tab[, 1:2], digits = 2))
      
      # automatische Zeilenumbrüche bei langen Labels ---------------------
      results[, 1] <- sapply(results[, 1], \(x) paste(strwrap(x, width = 40), collapse = "\n"))
      
      results <- results[!(rownames(results) %in% c("NA's", "Total")), ]
      colnames(results) <- c("label", "freq", "perc")

      if(show.plot == TRUE) {
        if(fig.height == "default")
        {subchunkify(barplot.sc.mc(results, xlab = "Häufigkeit"), fig_height = (1 + 0.75*nrow(results)), fig_width = 9)}
        else
        {subchunkify(barplot.sc.mc(x = results, xlab = "Häufigkeit"), fig_height = fig.height, fig_width = 9)}
      }
      if(no.pagebreak == TRUE) {
        cat("\n\n\n")
      }
      cat("   \n  \n")

    }
  }
}
