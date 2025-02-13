#' Testet, ob die Labels aus personalized.info so im Datensatz vorkommen
#'
#' @param col Spalte aus der Info-Tabelle, z.B. info$Fb.text
#' @param var Variable aus Datensatz, die der Spalte entspricht
#' @param expcetion Ausnahmen, die nicht überprüft werden sollen
#' 
#' @examples
#' # In diesem Fall wird die Variable "FB.text" aus der Info mit der Variable
#' # "Teilbereich" aus dem Datensatz verglichen und alles stimmt
#' label.test(BspDaten$pInfo$FB.txt, BspDaten$dataLVE$Teilbereich)
#' 
#' # So sieht es aus, wenn die Labels nicht komplett übereinstimmen:
#' label.test(BspDaten$pInfo$FB.txt.falsch, BspDaten$dataLVE$Teilbereich)
#' @export

# Testen, ob Labels aus personalized.info so im Datensatz vorkommen
label.test <- function(col, # Spalte aus der Info-Tabelle, z.B. info$Fb.text
                       var, # Variable aus Datensatz, die der Spalte entspricht
                       exception = "alle") { # Ausnahmen, die nicht überprüft werden sollen
  
  labels.col <- unique(col)
  
  if (length(exception != 0)) {
    for (k in 1:length(exception)) {labels.col <- labels.col[labels.col != exception[k]]}
  }
  
  labels.var <- sjlabelled::get_labels(var)
  
  
  if (all(labels.col %in% labels.var)) {
    output <- "Alle Labels der Spalte aus personalized.info kommen in gleicher Schreibweise auch in der Variable vor"
    
  } else {
    
    false.labels <- labels.col[which(!(labels.col %in% labels.var))]
    output <- paste0("Das Label \"", false.labels, "\" aus der Spalte von personalized.info kommt nicht in gleicher Schreibweise in den Labels der Variable vor.")
    
  }
  return(print(output))
}