#' Funktion für Ausweichoptionen
#'
#' @param x Daten
#' @param show.alt Zeige Ausweichoptionen, falls es sie gibt 
#' @param number Skala (OHNE AUSWEICHOPTIONEN!)
#' @param alt1 Text für erste Ausweichoption (standardmäßig 0 in den Daten, siehe alt1.num)
#' @param alt1.num 0
#'
#' @export text.alternative

text.alternative <- function(x, # Daten
                             show.alt = TRUE, # Zeige Ausweichoptionen, falls es sie gibt
                             number = 6, # Skala (OHNE AUSWEICHOPTIONEN!)
                             alt1 = FALSE, # Text für erste Ausweichoption (standardmäßig 0 in den Daten, siehe alt1.num)
                             alt1.num = 0) {
  
  if (show.alt == TRUE) {
    if(alt1 != FALSE) {
      
      cat("Die Ausweichoption *„", alt1, "“* wurde ", sum(x == alt1.num, na.rm = TRUE),
          " mal gewählt.  \n  \n", sep = "")
      
    }
    
    cat("  \n \n")
    
  }
}
