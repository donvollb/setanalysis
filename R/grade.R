#' merge-Funktion für Schulnoten
#'
#' @param x Daten
#' @param kennung Kennung/Fallnummer zum Aggregieren 
#' @param show.table Soll Tabelle gezeigt werden?
#' @param already.aggr Sind die Daten bereits aggregiert, bei TRUE wird nicht aggregiert
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#' @param nr Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
#'
#' @export

grade <- function(x, # Daten
                  kennung, # Kennung/Fallnummer zum Aggregieren
                  show.table = TRUE, # Soll Tabelle gezeigt werden?
                  already.aggr = FALSE, # Sind die Daten bereits aggregiert, bei TRUE wird nicht aggregiert
                  inkl = "nr", # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                  nr = "") # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
{
  if (inkl == "nr") {
    if (nr == "") {inkl <- TRUE} else {inkl <- eval(parse(text = paste0("inkl.", nr)))}
  }

  if (inkl == TRUE) {

    label <- attr(x, "label")

    if(already.aggr == FALSE) {
      x <- aggr.data(x, kennung)}

    cat("\\subsubsection{ ", attr(x, "label"), "}  \n  \n", sep = "")

    descr <- as.data.frame(psych::describe(x)) [c(2:5,8:9)]
    descr$Item <- label
    descr <- descr[c(7, 1:6)]
    if(sum(descr$n) == 0){cat("*Tabelle wurde wegen fehlender Daten nicht erstellt.*  \n  \n")} else {
      colnames(descr) <- c(paste0("\\textbf{Item} \\textit{[Skala: Schulnoten]}"),
                           "\\textbf{N\\textsubscript{courses}}",
                           "\\textbf{M}",
                           "\\textbf{SD}",
                           "\\textbf{Md}",
                           "\\textbf{Min}",
                           "\\textbf{Max}")
      kableExtra::kbl(descr, row.names = FALSE, booktabs = TRUE,
                      digits = 2,
                      longtable = TRUE,
                      format="latex",
                      linesep = "\\addlinespace",
                      escape = FALSE,
                      #      ) %>% column_spec(1, width = "230pt") -> table
      ) %>% column_spec(1, width = 81) -> table


      if(show.table == TRUE) {
        flextable_to_rmd(
          #        subchunkify(
          table.stat.multi(x,
                           #                           col1.name = "\\textbf{Item} \\textit{[Skala: Schulnoten]}",
                           col1.name = "Item [Skala: Schulnoten]",
                           #                           col2.name = "N\\textsubscript{courses}",
                           col2.name = "N",
                           bold.col1 = FALSE) %>%
            append_chunks(as_sub("courses"), i=1, j=2, part="header") %>% # courses tiefergestellt
            bold(i=1, j=1, part="header", bold=FALSE) %>%
            mk_par(
              i=1, j=1, part="header",
              as_paragraph(
                as_b("Item"),
                colorize(as_i(" [Skala: Schulnoten]"), color="gray20")
              )
            )

          #                           fig_height = 7, fig_width = 9
          #                    )
        )
      }

      subchunkify(boxplot.gesnote(x), fig_height = 2, fig_width = 9)


    }


  }
}
