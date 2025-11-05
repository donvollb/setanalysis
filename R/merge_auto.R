#' Funktion, die den automatisch Itemtyp erkennt und entsprechend auswertet
#'
#' @param x auszuwertende Daten
#' @param nr_auto Soll die Nummer automatisch ermittelt werden?
#' @param nr Manuelle Eingabemöglichkeit der Nummer
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#' @param ... Argumente zum „weitergeben“ in die Funktion
#'
#' @export merge_auto

merge_auto <- function(x,
                       nr_auto = TRUE,
                       nr = "",
                       inkl = "nr",
                       ...) { # Argumente zum „weitergeben“ in die Funktion


if (typeof(x) != "list") {
  type <- attr(x, "type")
} else {
  type <- paste0("multi.", attr(x[, 1], "type"))
}

if (isTRUE(nr_auto & nr == "" & inkl == "nr")) {
  if (type %in% c("multi.mc", "multi.sk")) {nr <- attr(x[, 1], "nr")
  } else {nr <- attr(x, "nr")}}
  
  
# Erkennung, ob es sich um offene oder numerische Fragen handelt
## Zuerst prüfen, ob es überhaupt Buchstaben gibt

if (type == "open/num" & typeof(x) != "character") {
  type <- "num"
}  

# Falls es Buchstaben gibt: Schätzung anhand des Anteils der Ziffern
if (type == "open/num") {
  
  # NA-Werte entfernen, Vektor in eine lange Zeichenkette umwandeln
  long <- paste(na.omit(x), collapse = "") 
  
  # Anzahl der Ziffern in der Zeichenkette zählen
  anzahl_ziffern <- length(gregexpr("[0-9]", long)[[1]])
  
  # Gesamtlänge der Zeichenkette speichern 
  gesamtlänge <- nchar(long)

  # Anteil berechnen
  anteil <- anzahl_ziffern / gesamtlänge
  
  if (anteil > 0.5) {type <- "num"} else {type <- "open"}
}

Funktionsliste <- list(
      sc = merge.sc,
      sk = merge.evasys.sk,
    open = merge_open,
     num = merge.num,
multi.mc = merge.mc,
multi.sk = merge.multi.sk
)

Funktionsliste[[type]](x, nr = nr, inkl = inkl, ...)
}
