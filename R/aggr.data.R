#' Testet, ob die Labels aus personalized.info so im Datensatz vorkommen
#'
#' @param vars Variablen (oder eine Variable), die aggregiert werden sollen
#' @param kennungen Die Kennungen (z. B. LV-Kennungen oder Fallnummern), nach denen die Daten aggregiert werden sollen
#' 
#' @export

# Funktion zum Aggregieren von Daten anhand einer Kennung/Fallnummer
aggr.data <- function(vars, # Variablen (oder eine Variable), die aggregiert werden sollen
                      kennung) # kennung können z.B. die LV-Kennungen oder die Fallnummern sein
{
  labels <- as.character(lapply(data.frame(vars), attr, which = "label"))
  x <- data.frame(data.frame(vars)[0, ])
  for (n in unique(kennung)) {
    vars.sub <- data.frame(data.frame(vars)[kennung == n, ])
    x[nrow(x)+1, ] <- vars.sub |>
      apply(2, as.numeric) |>
      apply(2, mean, na.rm = TRUE)
  }

  for (k in 1:ncol(x)) {
    attr(x[, k], "label") <- labels[k]
  }

  return(x)

}

