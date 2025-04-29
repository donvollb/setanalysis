#' merge-Funktion für Schulnoten
#'
#' @param x Daten
#' @param kennung Kennung/Fallnummer zum Aggregieren 
#' @param show.table Soll Tabelle gezeigt werden?
#' @param already.aggr Sind die Daten bereits aggregiert, bei TRUE wird nicht aggregiert
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#' @param nr Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
#'
#' @examples markdown.in.viewer(grade(BspDaten$dataLVE$Note,
#'                                    kennung = BspDaten$dataLVE$Kennung))
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
    
    cat("*** ", attr(x, "label"), "\n \n", sep = "")
      
    if(length(x) == 0){cat("**Tabelle wurde wegen fehlender Daten nicht erstellt.**  \n  \n")} 
      
      if(show.table == TRUE) {
        subchunkify(
          table.stat.multi(x,
                           labels = label,
                           col1.name = "Item [Skala: Schulnoten]",
                           col2.name = "N",
                           bold.col1 = FALSE) |>
            flextable::append_chunks(flextable::as_sub("courses"), i=1, j=2, part="header") |> # courses tiefergestellt
            flextable::bold(i=1, j=1, part="header", bold=FALSE) |>
            flextable::mk_par(
              i=1, j=1, part="header",
              flextable::as_paragraph(
                flextable::as_b("Item "),
                flextable::colorize(flextable::as_i("[Skala: Schulnoten]"), color="gray20")
              )
            )
        )
      }
      
      subchunkify(boxplot.gesnote(x), fig_height = 2, fig_width = 9)
      
    }
  }
