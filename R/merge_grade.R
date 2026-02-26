#' merge-Funktion für Schulnoten
#' `grade()` ist eine veraltete Schreibweise der gleichen Funktion
#'
#' @param x Daten
#' @param kennung Kennung/Fallnummer zum Aggregieren 
#' @param show.table Soll Tabelle gezeigt werden?
#' @param already.aggr Sind die Daten bereits aggregiert?
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#' @param nr Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
#'
#' @examples
#' merge_grade(BspDaten$dataLVE$Note,
#'             kennung = BspDaten$dataLVE$Kennung) |> markdown_in_viewer()
#' @export

merge_grade <- function(x, # Daten
                        kennung, # Kennung/Fallnummer zum Aggregieren
                        show.table = TRUE, # Soll Tabelle gezeigt werden?
                        already.aggr = FALSE, # Sind die Daten bereits aggregiert, bei TRUE wird nicht aggregiert
                        inkl = "nr", # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                        nr = "") # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
{
  if (inkl == "nr") {
    if (nr == "") {inkl <- TRUE} else {inkl <- eval(parse(text = paste0("inkl.", nr)))}
  }
  
  if (inkl != TRUE) {return(invisible())} # wenn inkl nicht TRUE ist, wird die Funktion beendet
    
  label <- attr(x, "label")
    
  if(already.aggr == FALSE) {
    x <- aggr_data(x, kennung)}
    
  if(length(x) == 0){cat("**Tabelle wurde wegen fehlender Daten nicht erstellt.** \n\n")
    return(invisible())} 
    
  if(show.table == TRUE) {
    subchunkify(
      table.stat.multi(x,
                       labels = label,
                       col1.name = 'Item #text(style: "italic", weight: "regular")[Skala: Schulnoten]',
                       col2.name = "N#sub[Courses]",
                       bold.col1 = FALSE)        
    )
  }
  subchunkify(boxplot_grade(x), fig_height = 2, fig_width = 9)
}


#' @noRd
#' @export grade

grade <- merge_grade