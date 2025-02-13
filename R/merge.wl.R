#' merge-Funktion für den Workload (funktioniert, sollte überarbeitet werden)
#'
#' @param ECTS ECTS der Daten
#' @param WL WL der Daten (ECTS und WL müssen gleiche Länge haben, d.h. entweder ist bereits beides aggregiert oder keins)
#' @param kennung Kennung/Fallnummer zum Aggregieren
#' @param already.aggr Sind die Daten bereits aggregiert?
#'
#' @export

merge.wl <- function(ECTS, # ECTS der Daten
                     WL, # WL der Daten (ECTS und WL müssen gleiche Länge haben, d.h. entweder ist bereits beides aggregiert oder keins)
                     kennung, # Kennung/Fallnummer zum Aggregieren
                     already.aggr = FALSE) # Sind die Daten bereits aggregiert?
{
  if(already.aggr == FALSE) {
    tmp <- data.frame(ECTS, WL)
    x <- tmp[0, ]

    for (n in unique(kennung)) {
      tmp.sub <- tmp[kennung == n, ]
      md <- median(tmp.sub[, 2], na.rm = TRUE)

      x[nrow(x)+1, ] <- c(tmp.sub[1,1], md)

    }}

  ECTS <- as.character(x[, 1])
  ECTS[is.na(ECTS)] <- "k.A."
  WL <- as.numeric(x[, 2])

  boxplot.workload(WL ~ ECTS, names(table(ECTS)),
                   c("0h", "1h", "2h", "3h", "4h", "5h", "6h", "7h", "8h", "9h", "10h", "11h", "12h", "mehr als\n12h"),
                   length(unique(ECTS)), 13, nums = as.data.frame(table(ECTS))[, 2])

}
