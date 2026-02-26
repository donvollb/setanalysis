#' merge-Funktion zum Zusammenfügen der single-choice Fragen nach dem 1. und 2. Fach
#'
#' @param x1 Daten von Fach 1
#' @param x2 Daten von Fach 2
#' @param inkl1 TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr1" zieht sich automatisch die entsprechende inkl. Variable
#' @param inkl2 TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr2" zieht sich automatisch die entsprechende inkl. Variable
#' @param nr1 Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
#' @param nr2 Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
#'
#' @examples
#' 
#' merge_subj(BspDaten$dataSHOWUP$fach1_2FB,
#'            BspDaten$dataSHOWUP$fach2_2FB) |> markdown_in_viewer()
#' 
#' @export merge_subj

merge_subj <- function(x1, # Daten von Fach 1
                       x2, # Daten von Fach 2
                       inkl1 = "nr1", # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr1" zieht sich automatisch die entsprechende inkl. Variable
                       inkl2 = "nr2", # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr2" zieht sich automatisch die entsprechende inkl. Variable
                       nr1 = "", # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
                       nr2 = "") # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
{


  # Überprüfung der inkl-Parameter ----------------------------------------

  if (inkl1 == "nr1") {
    if (nr1 == "") {inkl1 <- TRUE} else {inkl1 <- eval(parse(text = paste0("inkl.", nr1)))}
  }

  if (inkl2 == "nr2") {
    if (nr2 == "") {inkl2 <- TRUE} else {inkl2 <- eval(parse(text = paste0("inkl.", nr2)))}
  }

  if (inkl1 != TRUE | inkl2 != TRUE) {
    return(invisible())} # wenn nicht beide inkl TRUE sind, wird Funktion beendet

  
  # Zusammenfügen der beiden Fächer-Spalten -------------------------------
  
  subj <- rbind(data.frame(fach = unlist(x1, use.names = FALSE)),
                  data.frame(fach = unlist(x2, use.names = FALSE))) 

  # Vergabe des neuen Labels ----------------------------------------------
  
  attr(subj$fach, "label") <- paste0(" & ", nr2, " ",sub("\\?.*", "",
                                     attr(subj$fach, "label")), " / 2. Fach? ")
  
  # Aufruf der merge_sc-Funktion ------------------------------------------
  
  merge_sc(subj$fach, nr = nr1)

  cat(paste("*Hinweis: In der Befragung wurden 1. und 2. Fach getrennt abgefragt;",
            "in dieser Tabelle werden die Antworten gemeinsam dargestellt.",
            "Daraus ergibt sich in dieser Darstellung eine Verdopplung des",
            "Stichprobenumfangs (siehe „Total“).*  \n  \n"))
}


#' @noRd
#' @export merge.subj

merge.subj <- merge_subj