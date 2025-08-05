#' Funktion für die automatische Auswertung mehrerer verschiedener Items unterschiedlicher Typen 
#'
#' @param x Ausschnitt aus dem Datensatz, der ausgewertet werden soll
#' @param multi.sk Sollen aufeinanderfolgende sk-Items gemeinsam ausgewertet werden?
#' @param nr_auto Soll die Nummer automatisch ermittelt werden?
#' @param nr Manuelle Eingabemöglichkeit der Nummer
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#'
#' @export merge_many

merge_many <- function(x, # Ausschnitt aus dem Datensatz
                       multi.sk = TRUE, # Sollen aufeinanderfolgende sk-Items gemeinsam ausgewertet werden?
                       nr_auto = TRUE, # Soll die Nummer automatisch ermittelt werden?
                       nr = "", #
                       inkl = "nr"){
  
  
  # Prüfung, ob es überhaupt mehr als eine Spalte ist
  if(typeof(x) != "list") {
    return(merge_auto(x, nr_auto = nr_auto, nr = nr, inkl = inkl))}

  # Schleife für die Verarbeitung der Items
  for (i in seq_len(ncol(x))) {
    type <- attr(x[, i], "type")

    # Prüfen, ob es eine einzeln auszuwertende Spalte ist -----------------
    if (type %in% c("sc", "open/num") | (type == "sk" & isFALSE(multi.sk))) {
      merge_auto(x[, i], nr_auto = nr_auto, nr = nr, inkl = inkl)
      
    # Spezialbehandlung von mc-Items --------------------------------------
    } else if (type == "mc") {
      
      # Prüfen, ob die Nummer die gleiche ist wie bei der nächsten Spalte
      if (attr(x[, i], "nr") == attr(x[, i + 1], "nr")) {
        
        # Falls ja: Zähler einstellen und sonst nichts tun
        if(!exists("counter")) {counter <- 1} else {counter <- counter + 1}
        
        # Falls nein: Anhand des Zählers alle Spalten des mc-Items auswählen und
        # den Zähler danach wieder entfernen
      } else {
        merge_auto(x[, (i-counter):i], nr_auto = nr_auto, nr = nr, inkl = inkl)
        rm(counter)
      }
    
    # Spezialbehandlungen sk-Items (falls multi.sk TRUE ist) --------------
    } else if (type == "sk") {
      
      # Prüfen, ob die nächste Spalte auch ein sk-Item ist
      if (attr(x[, i + 1], "type") == "sk") {
      
        # Falls ja: Zähler einstellen und sonst nichts tun
        if(!exists("counter")) {counter <- 1} else {counter <- counter + 1}
        
        # Falls nein: Anhand des Zählers alle aufeinanderfolgenden sk-Items
        # auswählen und den Zähler danach wieder entfernen
      } else {
        merge_auto(x[, (i-counter):i], nr_auto = nr_auto, nr = nr, inkl = inkl)
        rm(counter)
      }
    
    # Fehlermeldungen, falls der Typ nicht stimmt   -----------------------
    } else if(!is.null(type)) {
      stop(paste0("Die Spalte „", names(x)[[i]],
                "“ hat den nicht unterstützten Typ „", type, "“."))
    } else { stop("Die Spalte „", names(x)[[i]],
                "“ hat keinen Typ (Typen sind z. B. „sc“, „open“, „sk“).")}
    
  }
}
  

### Idee: Automatische Seitenumbrüche -------------------------------------