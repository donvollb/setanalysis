

### ÄÖÜ richtig angezeigt? - Reopen with Encoding -> UTF-8 ###

# Teständerung
# Teständerung 2

# MS 16.10.2024: Aenderung an der merge.sc-Funktion für Kompatibilitaet mit Quarto
# Hintergrund: {cat("\\begin{minipage}{\\linewidth} \n")} und cat("\n\\end{minipage}") fuehren zu Problemen beim Kompilieren
# Loesung: Ersetzung durch {cat("\\pagebreak  \n  \n")} fuehrt zu identischem Ergebnis und funktioniert in Quarto

# MS 29.10.2024: Entfernen von fig.align = \"center\" in Subchunkify-Funktion
# Hintergrund: Fuehrt zu zu vielen Div-Containern in Quarto (bzw. .md-Datei), wobei Oeffnen und Schließen teils zusammenlaufen
# und so zu Problemen bei der weiteren Verarbeitung fuehren
# Loesung: fig.align='center' als Chunk-Optionen in .qmd-Datei setzen (1 Div-Container fuer ganzen Chunk)

# MS 05.11.2024: kableExtra::kbl Tabellen durch Flextable ersetzen
# betroffene Funktionen: lv.kable
# Aufruf der Flextable mit flextable_to_rmd() in Funktionen boxplot.ruecklauf, merge.fachsem, merge.multi.sk, merge.sc, grade



######### LV: Funktionen Berichte #########


#### Setup ####
# Pakete laden
suppressPackageStartupMessages(if(!require(pacman)){install.packages("pacman")})
pacman::p_load(ggplot2, knitr, psych, descr, dplyr, kableExtra, cowplot, flextable, gdtools, officer
               #fontspec
               )

# jede Spalte bei Tabellen erhöht die Breite um 12pt durch automatischen Abstand

# Globale Variablen festsetzen, sofern sie (beim Einlesen) noch nicht vorhanden sind
# Idee: Sollte z.B. die Farbe für die Balken (color.bars) nicht schon im Skript festgelegt sein, passiert dies hier, damit die Funktionen darauf zugreifen können
# DIESE VARIABLEN HIER NICHT VERÄNDERN!

# Schriftart (font.family)
if(!exists("font.family")){font.family <- c("Red+Hat+Text")}

# Spaltenbreiten (die Summen der einzelnen Spaltenbreiten sind unterschiedlich groß, da jede Spalte automatisch 12pt hinzufügt) 
# für Flextable alle Spaltenbreiten umgerechnet von pt zu mm
# 3 Spalten
#if(!exists("col.width3")){col.width3 <- c("305pt", "50pt", "30pt")}
if(!exists("col.width3")){col.width3 <- c(108, 18, 11)}
# 4 Spalten
#if(!exists("col.width4")){col.width4 <- c("243pt", "50pt", "30pt", "50pt")}
if(!exists("col.width4")){col.width4 <- c(86, 18, 11, 18)}
# 7 Spalten (für table.stat.multi)
#if(!exists("col.width.sm")){col.width.sm <- c("182pt", "30pt", "25pt", "25pt", "25pt", "25pt", "25pt")}
if(!exists("col.width.sm")){col.width.sm <- c(64, 11, 9, 9, 9, 9, 9)}
# 8 Spalten (für table.stat.multi mit einer Ausweichoption)
#if(!exists("col.width.sm.alt1")){col.width.sm.alt1 <- c("168pt", "22pt", "22pt", "22pt", "22pt", "22pt", "22pt", "25pt")}
if(!exists("col.width.sm.alt1")){col.width.sm.alt1 <- c(59, 8, 8, 8, 8, 8, 8, 9)}
# 9 Spalten (für table.stat.multi mit zwei Ausweichoptionen)
#if(!exists("col.width.sm.alt2")){col.width.sm.alt2 <- c("152pt", "21pt", "18pt", "18pt", "18pt", "18pt", "18pt", "25pt", "25pt")}
if(!exists("col.width.sm.alt2")){col.width.sm.alt2 <- c(54, 7, 6, 6, 6, 6, 6, 6, 9)}
# Erste Spalte von table.stat.single (Zum Beispiel breiter machen, wenn N_courses_ statt N)
#if(!exists("col1.width.tss")){col1.width.tss <- "25pt"}
if(!exists("col1.width.tss")){col1.width.tss <- 12}

# Farben
# Balken
if(!exists("color.bars")){color.bars <- rgb(109, 172, 220, maxColorValue = 255)}
# Schriftarten
if(!exists("color.font")){color.font <- "steelblue"}
# Farbe der "Streifen" der Tabellen
if(!exists("table.color")){table.color <- "tablecolor"}

# Abbildungen
# SC-Fragen
if(!exists("show.plot.sc")){show.plot.sc <- TRUE}
# MC-Fragen
if(!exists("show.plot.mc")){show.plot.mc <- TRUE}
# Skalen-Fragen
if(!exists("show.plot.sk")){show.plot.sk <- TRUE}

# Environments
if(!exists("list.open")){list.open <- new.env()}

# Globale inkl-Variable für offene Fragen
if(!exists("inkl.open")){inkl.open <- TRUE}


#### ELEMENTARE FUNKTIONEN ####


# Erzeugen von Sub-Chunks
subchunkify <- function(g, # Code (kann auch mit Aufzählung ("c(...)") benutzt werden)
                        fig_height=7, # figure height des Sub-Chunks
                        fig_width=5, # figure width des Sub-Chunks
                        hide = FALSE) # "hide" für den Sub-Chunk
{
  g_deparsed <- paste0(deparse(
    function() {g}
  ), collapse = '')
  
  
  # Nachfolgend entfernt: fig.align = \"center\"
  # Sonst zu viele Div-Container, die fuer Probleme in Quarto sorgen
  # Stattdessen Zentrierung als Chunk-Option in .qmd-Datei
  #if(hide == FALSE) {head.end <- ", echo=FALSE, results = \"asis\", fig.align = \"center\", out.width = \"100%\"}"} 
  if(hide == FALSE) {head.end <- ", echo=FALSE, results = \"asis\",  out.width = \"100%\"}"} 
  #else {head.end <- ", echo=FALSE, results = \"hide\", fig.keep = \"all\", fig.align = \"center\", out.width = \"100%\"}" }
  else {head.end <- ", echo=FALSE, results = \"hide\", fig.keep = \"all\", out.width = \"100%\"}" }
  
  if(!exists("sub.nr")) {assign("sub.nr", 0, envir = globalenv())}
  assign("sub.nr", sub.nr+1, envir = globalenv())
  
  sub_chunk <- paste0("```{r sub_chunk_", sub.nr, ", fig.height=", fig_height, ", fig.width=", fig_width, head.end,
                      "  \npar(family = \"", font.family, "\")  \n",
                      "  \n", 
                      "\n(", 
                      "  \n", 
                      g_deparsed
                      , ")()",
                      
                      "\n```")
  
  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
}


# Automatische Zeilenumbrüche
auto.newline <- function(x, # Objekt
                         number = 47) # Anzahl an maximalen Zeichen pro Zeile, 47 ist optimal für SC/MC-Plots
{
  for (i in 1:length(x)) {
    if(nchar(x[i]) > number-1) {
      split <- unlist(strsplit(x[i], paste0("(?<=.{", number, "})"), perl = TRUE))
      
      
      for (k in 1:length(split)){
        
        for (l in 1:10) {
          if (nchar(split[k]) > number-1) {
            
            
            split.rev <- paste0(rev(strsplit(split[k], "")[[1]]), collapse = "")
            split.new <- sub(" ", " TEGRAT", split.rev)
            split.target <- paste0(rev(strsplit(split.new, "")[[1]]), collapse = "")
            splits <- unlist(strsplit(split.target, "TARGET"))
            
            split[k] <- splits[1]
            split[k+1] <- paste0(splits[2], split[k+1])
            
          }}
        
        
        
        
      }
      if (grepl("NA$", split[length(split)]) == TRUE) {split[length(split)] <- sub("NA$", "", split[length(split)])}
      split <- gsub("^ ", "\n", split)
      x[i] <- paste(split, collapse = "") } }
  
  x
  
}

# Alternativ-Funktion für Zeilenumbrüche
auto.newline2 <- function(x, # Objekt
                          number = 15) # Anzahl an maximalen Zeichen pro Zeile, 47 ist optimal für SC/MC-Plots
{
  for (i in 1:length(x)) {
    if(nchar(x[i]) > number-1) {
      x[i] <- gsub(paste0("(.{1,", number, "})(\\s|$)"), "\\1\n", x[i])
      x[i] <- sub("\\n$", "", x[i])}}
  return(x)
}


# Labels für MC-Fragen aus dem Fragetext ziehen: 
get.label <- function(x, # Objekt
                      match = ": ") # String, der Label von Frage trennt
{
  sub(paste0(".*", match), '', attr(x, "label"))
}


# Tabellenerstellung (Abkürzung von kable)
lv.kable <- function(x, # Objekt (am besten dataframe)
                     caption = NULL, # caption der Tabelle
                     col.width = "default", # Spaltenbreite (Vektor, z.B. "c("30pt", "50pt")), bei "deafult" gibt es automatische Spaltenbreiten
                     bold = TRUE, # Soll der header fett sein?
                     bold.col1 = TRUE, # Soll die erste Zelle des Headers fett sein? Sinnvoll, falls alle bis auf erste Zelle fett sein sollen
                     digits = 2, # Wie viele Nachkommastellen in der Tabelle?
                     escape = FALSE, # escape Argument in kbl
                     striped = TRUE) # gestrifte Tabelle?
{
#  if(ncol(x) != length(col.width) & col.width[1] != "default") {
#    stop(paste0("Es gibt eine unterschiedliche Anzahl an definierten Spaltenbreiten (col.width), nämlich ", 
#                length(col.width), " und Spalten in der Tabelle, nämlich ", ncol(x), "."))
#  }
  
#  if (escape == FALSE) {
#    if(bold == TRUE & bold.col1 == TRUE) {colnames(x) <- paste0("\\textbf{", colnames(x), "}")}
#    if(bold == TRUE & bold.col1 == FALSE) {colnames(x)[-1] <- paste0("\\textbf{", colnames(x)[-1], "}")}
#  }
  
  
#  latex.table <-  suppressWarnings(kableExtra::kbl(x, 
#                                                   booktabs = TRUE,
#                                                   longtable = TRUE,
#                                                   format = "latex",
#                                                   digits = digits,
#                                                   linesep = "\\addlinespace",
#                                                   escape = escape,
#                                                   caption = caption,
#                                                   row.names = FALSE))
 
  
  
  
  
# Flextable statt kableExtra::kbl Tabellen
  
# Erstellen einer Flextable  
  latex.table <-  flextable::flextable(x)
  
# Überschreiben der Default-Einstellungen
  set_flextable_defaults( 
    font.family = "Red Hat Text",
    hansi.family = "Red Hat Text",
    eastasia.family = "Red Hat Text",
    cs.family = "Red Hat Text",
    big.mark = "",
    na_str = "NA",
    font.size = 10
  )
  
# Bisherige Beobachtung für Schriftart (Google Font, z.B. Red Hat Text) bei Flextable
# 1. Schriftart muss lokal "Für alle Benutzer" installiert werden (damit Pfad C:\Windows\Fonts)
# 2. Bei Red Hat Text sollten nur "Regular", "Italic", "Bold" installiert werden, sonst ist der fette Text in
# Flextable nicht ganz Fett (nur "Medium" oder "SemiBold"?)
# Monospace Fonts, die funktionieren: Courier New, Lucida Sans Typewriter
#  gdtools::register_gfont(family = "Red Hat Text", subset = c("latin", "latin-ext")) # Google Font registrieren
  latex.table <-  font( # neben defaults nochmal aufrufen, sonst ist Legenden-Tabelle in Arial (?)
    latex.table,
    i = NULL,
    j = NULL,
    fontname = "Red Hat Text",
    part = "all",
  )
  
# Schriftgröße anpassen
  latex.table <- fontsize( # neben defaults nochmal aufrufen, sonst ist Legenden-Tabelle in 11 pt (?)
    latex.table,
    i = NULL,
    j = NULL,
    size = 10,
    part = "all")
  
# Striped Theme
  latex.table <- theme_zebra(
    latex.table,
    odd_header = "#ecf1f4", # RPTU blaugrau (10 %)
    odd_body = "transparent",
    even_header = "transparent",
    even_body = "#ecf1f4" # RPTU blaugrau (10 %)
  )



  
  
# Fallunterscheidung für Spaltenbreiten
# keine Spaltenbreite angegeben: autofit
# Spaltenbreite angegeben: fixed und width() verwenden
  if (col.width[1] == "default") {
    # Wenn keine Spaltenbreite angegeben: autofit
    latex.table <- set_table_properties(
      latex.table,
      layout = "autofit", # fixed (Spaltenbreite in width()-Funktion wird verwendet) oder autofit (Tabellenbreite width() innerhalb dieser Funktion wird verwendet; nicht für PDFs)
      width = 0, # kein Einfluss bei PDF; wird ignoriert wenn layout="fixed"
      align = NULL,
      opts_pdf = list(
        tabcolsep = 5 # Padding links und rechts für PDFs (latextable::padding nicht für PDF)
      )
    )
  }
  if (col.width[1] != "default"){
    # wenn Spaltenbreite festgelegt: fixed
    latex.table <- set_table_properties(
      latex.table,
      layout = "fixed", # fixed (Spaltenbreite in width()-Funktion wird verwendet) oder autofit (Tabellenbreite width() innerhalb dieser Funktion wird verwendet; nicht für PDFs)
      width = 0, # kein Einfluss bei PDF; wird ignoriert wenn layout="fixed"
      align = NULL,
      opts_pdf = list(
        tabcolsep = 5 # Padding links und rechts für PDFs (latextable::padding nicht für PDF)
      )
    )
    # Spaltenbreite anpassen
    latex.table <- width(
      latex.table,
      width = col.width, # von pt-Angaben in Quarto-Dokument grob umgerechnet in mm
      unit = "mm"
    ) # nur cm, mm, in
  }
  
  
  
  
# Horizontale Rahmelinien
  latex.table <- hline_top(
    latex.table,
    border = officer::fp_border(color = "black", width = 1),
    part = "header")
  latex.table <- hline_top(
    latex.table,
    border = officer::fp_border(color = "black", width = .5),
    part = "body")
  latex.table <- hline_bottom(
    latex.table,
    border = officer::fp_border(color = "black", width = 1),
    part = "body")

  
  

  
  
  
  
  
  
#  if (col.width[1] != "default") {
#    for (k in 1:length(col.width)) {
#      latex.table <- column_spec(latex.table, k, width = col.width[k], latex_valign = "m")
#    }  }
  
#  if (striped == TRUE) {
#    latex.table <- row_spec(latex.table, row = seq(from = 0, to = nrow(x), by = 2), background = table.color)
#  }
  
#  if(bold == TRUE & escape == TRUE) {latex.table <- row_spec(latex.table, 0, bold=TRUE)}
  
#  if(bold.col1 == FALSE) {latex.table <- column_spec(latex.table, 1, bold = FALSE)}
  
  

# Erstellte kable-Tabelle in Flextable umwandeln
#  return(
#    flextable::as_flextable(latex.table) # as_flextable nicht anwendbar auf Objekt der Klasse "knitr_kable"
#    )
  
#  return(latex.table)
  latex.table
   
}


# Ersetzen von "problematischen" Zeichen für LaTex
replace.latex.issues <- function(x, all = TRUE) #Objekt
{
  
  if(all == TRUE) {
    x <- gsub("&", "\\\\&", x)
    x <- gsub("_", "\\\\_", x)
    x <- gsub("'", "\\'", x)
    x <- gsub("%", "\\\\%", x)
  } else {
    #  x <- gsub("  ", " ", x)
    for (f in 1:5) {x <- gsub("\\.$", "", x)} 
  }
  
  for (k in 1:length(x))
  {if(grepl("^\\[", x[k])) {
    x[k] <- gsub("^\\[", "\\{\\[", x[k])
    x[k] <- paste0(x[k], "}")}}
  
  for (k in 1:length(x))
  {if(grepl("^\\(", x[k])) {
    x[k] <- gsub("^\\(", "\\{\\(", x[k])
    x[k] <- paste0(x[k], "}")}}
  
  
  return(x)
  
}


# Funktion zum Aggregieren von Daten anhand einer Kennung/Fallnummer
aggr.data <- function(vars, # Variablen (oder eine Variable), die aggregiert werden sollen
                      kennung) # kennung können z.B. die LV-Kennungen oder die Fallnummern sein
{
  labels <- as.character(lapply(data.frame(vars), attr, which = "label"))
  x <- data.frame(data.frame(vars)[0, ])
  for (n in unique(kennung)) {
    vars.sub <- data.frame(data.frame(vars)[kennung == n, ])
    x[nrow(x)+1, ] <- vars.sub %>%
      apply(2, as.numeric) %>%
      apply(2, mean, na.rm = TRUE)
  }
  
  for (k in 1:ncol(x)) {
    attr(x[, k], "label") <- labels[k]
  }
  
  return(x)
  
}

# Funktion für Fragen mit offenem Antwortformat (benötigt u.a. "list.open", wird am Anfang des Skripts erstellt)
open.answers <- function(x, # Daten 
                         inkl = "nr",  # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                         inkl.global = inkl.open, # Zweite inkl-Variable, die die globale Variable "inkl.open" abfragt. Kann auch in TRUE oder FALSE geändert werden
                         nr = "", # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
                         freq = FALSE, # Sollen gleiche offene Antworten zusammengefasst werden? Dann werden auch Häufigkeiten angezeigt
                         no.pagebreak = TRUE) # Seitenumbrüche mittendrin verhindern?
{
  
  
  
  if (inkl == "nr") {
    if (nr == "") {inkl <- TRUE} else {inkl <- eval(parse(text = paste0("inkl.", nr)))}
  }
  
  if (inkl == TRUE && inkl.global == TRUE) {
    if(no.pagebreak == TRUE) {cat("\\begin{minipage}{\\linewidth} \n")}
    if(!exists("anchor.nr")) {assign("anchor.nr", 0, envir = globalenv())}
    assign("anchor.nr", anchor.nr+1, envir = globalenv())
    
    
    anchor.top <- paste0(anchor.nr, ".top")
    anchor.bottom <- paste0(anchor.nr, ".bottom")
    
    cat(paste0("\\hypertarget{", anchor.top, "}{}\\textbf{", nr, " ", replace.latex.issues(attr(x, "label")), "}  \n  \n"))
    
    if(length(na.omit(x)) > 0) {
      cat(paste0("\\textit{Die offenen Antworten zu dieser Frage finden sich im \\hyperlink{", anchor.bottom, "}{Anhang}.}  \n \n"))
    } else {
      cat("\\textit{Keine offenen Antworten zu dieser Frage.}  \n  \n")
    }
    if(no.pagebreak == TRUE) {
      cat("\n\\bigskip")
      cat("\n\\end{minipage}")
    }
    cat("   \n  \n")
    
  }
  
  assign(paste0("var.", anchor.nr), x, envir = list.open)
  assign(paste0("nr.", anchor.nr), nr, envir = list.open)
}


# Testen, ob Labels aus personalized.info so im Datensatz vorkommen
label.test <- function(col, # Spalte aus personalizd.info, z.B. personalized.info$Fach
                       var, # Variable aus Datensatz, die der Spalte entspricht
                       exception = "alle") { # Ausnahmen, die nicht überprüft werden sollen
  
  labels.col <- unique(col)
  
  if (length(exception != 0)) {
    for (k in 1:length(exception)) {labels.col <- labels.col[labels.col != exception[k]]}
  }
  labels.var <- sjlabelled::get_labels(var)
  
  
  if (all(labels.col %in% labels.var)) {
    output <- "Alles Labels der Spalte aus personalized.info kommen in gleicher Schreibweise auch in der Variable vor"
    
  } else {
    
    false.labels <- labels.col[which(!(labels.col %in% labels.var))]
    output <- paste0("Das Label \"", false.labels, "\" aus der Spalte von personalized.info kommt nicht in gleicher Schreibweise in den Labels der Variable vor.")
    
  }
  return(print(output))
}



#### BEISPIELE/LEGENDEN ####


# Beispiel-Boxplot mit Beschriftung
bsp.boxplot <- function(x = "default", # Daten, bei "default" wird ein Beispieldatensatz genutzt
                        color = color.bars, # Farbe des Boxplots
                        family = font.family)  # Schriftart
{
  
  if(x[1] == "default") {
    x <- c(2.1, 3.9, 3.9, 3.9, 3.9, 4.4, 4.4, 4.5, 4.5, 4.7, 4.7, 5, 5, 5.2, 5, 5.5, 5.5, 5.5, 5.5, 5.7, 5.7, 5.7, 5.7, 5.7)
  }
  
  subchunkify(c(
    opar <- par(no.readonly = TRUE),
    par(mar=c(4.8,11,4.1,6)), #bltr
    par(fg="gray50"), # Farbe Rand 
    par(family = font.family),
    boxplot(x, horizontal = TRUE, ylim=c(1,6), col = c(color.bars), 
            border="black", xaxt="n", axes = TRUE, pars=list(outcol= color.bars, outpch=20)),
    abline(v=c(1,2,3,4,5,6), col = "gray80"),
    boxplot(x, horizontal = TRUE, ylim=c(1,6), col = c(color.bars) , 
            border="black", xaxt="n", axes = TRUE, pars=list(outcol= color.bars, outpch=20), add = TRUE),
    mtext(c("trifft gar \nnicht zu", "", "", "", "", "trifft voll \nzu"), 
          side=1, line=1, at=c(1, 2, 3, 4, 5, 6), las=1, col="gray30", cex=1, font = 2),
    mtext(c("BOXPLOT-BEISPIEL"), side=2, line=2, at=c(1), las=1, col=c(color.font), cex=1, font = 2),
    par(xpd=TRUE),
    text(x=4.5, y=2,label="Median", col = "grey30"),
    segments(x0 = 4.5, y0 = 1.85, x1 = 5, y1 = 1.2, col = "grey30", lwd = 1),
    text(x=4, y= -0.4,label="6-stufige Skala", col = c(color.font)),
    segments(x0 = 4.8, y0 = -0.35, x1 = 5.5, y1 = -0.05, col = c(color.font), lwd = 1),
    segments(x0 = 1.4, y0 = -0.01, x1 = 3.35, y1 = -0.4, col = c(color.font), lwd = 1),
    text(x=1.5, y= 2,label="Ausreisser", col = "grey30"),
    segments(x0 = 2.1, y0 = 1.05, x1 = 1.5, y1 = 1.85, col = "grey30", lwd = 1),
    text(x=5.5, y= 2,label="Max", col = "grey30"),
    segments(x0 = 5.7, y0 = 1.05, x1 = 5.5, y1 = 1.85, col = "grey30", lwd = 1)), 
    fig_height = 2.7, # optimal: 2.7
    fig_width = 10, # optimal: 10
    hide = TRUE) # damit nicht Text dazu "ausgespuckt" wird
  
  cat("  \n  \n")
  
  
}


# Beispiel einer Statistik-Tabelle
bsp.table.stat <- function(all = TRUE)  # all = TRUE für eine Tabelle mit "Frage" und "Median", eher für LVE
{
  if(all == TRUE) {
    
    jim <- as.data.frame(cbind("Frage", "Häufigkeit", "Mittelwert", "Standard-\nabweichung", "Median", "kleinster beob. Wert", "größter beob. Wert"))
    colnames(jim) <- c("Item", "n", "M", "SD", "MD", "Min", "Max")
#    tab <- lv.kable(jim, col.width = c("30pt", "50pt", "50pt", "60pt", "35pt", "60pt", "60pt"))
    tab <- lv.kable(jim, col.width = c(11, 18, 18, 21, 12, 21, 21))
    
  } else {
    
    jim <- as.data.frame(cbind("Häufigkeit", "Mittelwert", "Standard-\nabweichung", "kleinster beob. Wert", "größter beob. Wert"))
    colnames(jim) <- c("n", "M", "SD", "Min", "Max")
#    tab <- lv.kable(jim, col.width = c("50pt", "50pt", "60pt", "60pt", "60pt"))
    tab <- lv.kable(jim, col.width = c(18, 18, 21, 21, 21))
  }
  
  return(tab)
  
}


# Beispiel einer Sechserskala
bsp.evasys.sk6 <- function(x = "default") # Daten, bei "default" wird ein Beispieldatensatz genutzt
{
  if(x[1] == "default") {
    x <- c(1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6)
  }
  
  tmin <- "linker Pol"
  tplu <- "rechter Pol"
  par(family = font.family)
  
  bobby <- x %>% 
    psych::describe(.) %>% 
    round(.,2) %>% 
    data.frame() %>%
    dplyr::select(n, mean, sd, min, max) %>% 
    data.frame()
  
  subchunkify(c(
    
    par(mar=c(5.1,10,4.1,10)),
    par(fg="gray50"), # Farbe Rand 
    barplot(rep(NA,length(table(x))),ylim=c(0,sum(table(x))),axes=FALSE),
    abline(v=c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7), col = "grey80"),
    bp <- barplot(table(x), 
                  ylim=c(0, sum(table(x))), 
                  col = color.bars,
                  axes = FALSE, add = TRUE),
    box(),
    
    axis(side = 3, at=bp, tick = FALSE, labels = paste(round(100*prop.table(table(x)),1), " %", sep="")),
    par(new=TRUE),
    par(family = font.family),
    bxp <- boxplot(as.numeric(x), plot=FALSE),
    bxp$stats <- matrix(c((bobby$mean-bobby$sd), bobby$mean, bobby$mean, bobby$mean, (bobby$mean+bobby$sd))),
    invisible(ifelse(bxp$stats[5,1]>6, bxp$stats[5,1] <- 6, bxp$stats[5,1] <- bxp$stats[5,1])),
    invisible(ifelse(bxp$stats[1,1]<1, bxp$stats[1,1] <- 1, bxp$stats[1,1] <- bxp$stats[1,1])),
    bxp(bxp, horizontal = TRUE, ylim=c(0.6,6.4), xlim = c(0.3,1.3), boxcol = rgb(0.55, 0, 0), staplewex = 0.6, staplelwd=2,
        boxlwd=3, 
        whisklty = 1, whisklwd=2, outline = FALSE, axes = FALSE),
    mtext(tmin, side=1, at = -0.1, line = -2.6, font = 2, col = "gray30"),
    mtext(tplu, side=1, at = 7.2, line = -2.6, font = 2, col = "gray30"),
    par(xpd=TRUE, family = font.family),
    text(x=0, y=2,label="Relative Häufigkeit der Antworten", col = "black"),
    segments(x0 = 0, y0 = 1.85, x1 = 0.75, y1 = 1.7, col = "gray", lwd = 2),
    text(x=2, y=2,label="Std.-Abw.", col = "black"),
    segments(x0 = 2, y0 = 1.85, x1 = 2.35, y1 = 1.2, col = "gray", lwd = 2),
    text(x=3, y=2,label="Mittelwert", col = "black"),
    segments(x0 = 3, y0 = 1.85, x1 = 3.5, y1 = 1.2, col = "gray", lwd = 2),
    text(x=0, y= -0.4,label="Skala", col = "black"),
    segments(x0 = 0, y0 = -0.25, x1 = 0.9, y1 = -0.1, col = "gray", lwd = 2),
    text(x=4, y= -0.4,label="Säulendiagramm", col = "black"),
    segments(x0 = 4, y0 = -0.25, x1 = 3, y1 = 0.4, col = "gray", lwd = 2)), 
    fig_height = 2.7, fig_width = 10, hide = TRUE)
  
  cat("  \n  \n")
}



#### TABELLEN ####


# Einfache Häufigkeitstabelle
table.freq <- function(x, # Daten
                       caption = NULL, # caption der Tabelle (siehe lv.kable)
                       cutoff = FALSE, # Soll es einen "cutoff" geben? z.B. werden bei 12 alle Werte >= 12 in "12 oder höher" dargestellt 
                       show.all = TRUE, # Bei TRUE werden auch nicht gewählte Antwortoptionen angezeigt
                       col1.name = "", # Name der ersten Zelle des headers
                       col2.name = "N", # Name der zweiten Zelle des headers
                       col.width = "default", # Spaltenbreite (siehe lv.kable)
                       order.table = FALSE, # Soll nach Häufigkeit sortiert werden? "decreasing" für absteigendes Sortieren
                       bold = TRUE, # fetter header? (siehe lv.kable)
                       bold.col1 = TRUE) # fette erste Zelle des headers? (siehe lv.kable)
{
  
  jim <- data.frame(round(descr::freq(x, plot=FALSE), 1))
  jim <- data.frame(rownames(jim), jim)
  rownames(jim) <- NULL
  
  jim[, 1] <- replace.latex.issues(jim[, 1])
  
  jim[jim == "NA's"] <- "NAs"
  
  
  if(show.all == FALSE) 
  {
    jim <- jim[jim[, 2] != 0 & jim[, 2] != "0", ] # Falls nur gewählte Optionen angezeigt werden sollen
  }
  
  names.jim <- c(col1.name,
                 col2.name, 
                 "%", 
                 "gültige %")
  
  if(length(jim) == 4){
    colnames(jim) <- names.jim
  } else{
    colnames(jim) <- names.jim[1:3]
  }
  
  if (is.numeric(x) == TRUE) {
    if (cutoff != FALSE & max(x, na.rm = TRUE) == cutoff){jim[nrow(jim)-2, 1] <- paste0(cutoff, " oder höher")}
  }
  
  
  
  if (order.table != FALSE) {
    
    decreasing <- ifelse(order.table == "decreasing", TRUE, FALSE)
    jim <- jim[c(order(jim[1:(nrow(jim)-ncol(jim)+2), 2], 
                       decreasing = decreasing), 
                 (nrow(jim)-ncol(jim)+3):nrow(jim)
    ), ]
  }
  
  if (col.width[1] == "default" & length(jim) == 4) {col.width <- col.width4}
  if (col.width[1] == "default" & length(jim) == 3) {col.width <- col.width3}
  
  lv.kable(jim, caption = caption, col.width = col.width, bold = bold,
           bold.col1 = bold.col1)
  
}


# Einfache Statistiktabelle für ein Item ohne Fragetext in Tabelle
table.stat.single <- function(x, # Daten
                              caption = NULL, # caption der Tabelle (siehe lv.kable)
                              md = FALSE, # Mit Median?
                              col1.name = "N_votes", # Name der ersten Zelle des headers
                              col1.width = col1.width.tss, # Breite der ersten Zeile
                              bold = TRUE, # fetter header? (siehe lv.kable)
                              bold.col1 = TRUE) # fette erste Zeile im header? (siehe lv.kable)
{
  
  if (md == FALSE) {
    bob <- data.frame(round(psych::describe(x),2))[c(2:4, 8:9)]
    colnames(bob) <- c(col1.name, "M", "SD", "Min", "Max")
    
    lv.kable(bob, caption = caption, 
#             col.width = c(col1.width, "25pt", "25pt", "25pt", "25pt"), 
             col.width = c(col1.width, 9, 9, 9, 9), 
             bold = bold,
             bold.col1 = bold.col1) } else {
               
               bob <- data.frame(round(psych::describe(x),2))[c(2:5, 8:9)]
               colnames(bob) <- c(col1.name, "M", "SD", "MD", "Min", "Max")
               
               lv.kable(bob, caption = caption, 
#                        col.width = c(col1.width, "25pt", "25pt", "25pt", "25pt", "25pt"), 
                        col.width = c(col1.width, 9, 9, 9, 9, 9), 
                        bold = bold,
                        bold.col1 = bold.col1)
             }
}


# Einfache Statistiktabelle für mehrere Items mit Fragetexten
table.stat.multi <- function(x, caption = NULL, # caption der Tabelle (siehe lv.kable)
                             col1.name = "Item", # Name der ersten Zelle des headers
                             col2.name = "N_votes", # Name der zweiten Zelle des headers
                             alt1 = FALSE, # Text für erste Ausweichoption 
                             alt2 = FALSE, # Text für zweite Ausweichoption
                             alt1.list = NULL, # Antworthäufigkeiten erste Ausweichoption
                             alt2.list = NULL, # Antworthäufigkeiten zweite Ausweichoption
                             bold = TRUE, # fetter header? (siehe lv.kable)
                             bold.col1 = TRUE, # fette erste Zeile im header? (siehe lv.kable)
                             labels = "labels") # Fragetexte, bei "labels" werden die labels der Variablen genommen
{
  
  if(labels == "labels") {labels <- as.character(lapply(x, attr, which = "label"))}
  
  bob <- as.data.frame(round(psych::describe(x), digits = 2))[c(2:5,8:9)]
  bob <- cbind(labels, bob)
  bob[, 1] <- replace.latex.issues(bob[, 1])
  colnames(bob) <- c(col1.name, 
                     col2.name, 
                     "M", 
                     "SD", 
                     "MD", 
                     "Min", 
                     "Max")
  
  widths <- col.width.sm
  
  if (alt1 != FALSE) {
    bob <- cbind(bob, alt1.list)
    colnames(bob)[length(colnames(bob))] <- alt1
    widths <- col.width.sm.alt1
  }
  
  if (alt2 != FALSE) {
    
    if (alt1 == FALSE) {stop("alt1 ist FALSE, alt2 aber nicht. Bitte bei nur einer Ausweichoption alt1 verwenden.")}
    bob <- cbind(bob, alt2.list)
    colnames(bob)[length(colnames(bob))] <- alt2
    widths <- col.width.sm.alt2
  }
  
  
  lv.kable(bob, caption = caption, 
           col.width = widths, 
           bold = bold,
           bold.col1 = bold.col1)
}



#### BARPLOTS ####


# Barplot zur Abbildung von Häufigkeiten (kann auch für Balkendiagramme bei ordinalen Skalennivaus genutzt werden)
barplot.freq <- function(x, # Daten
                         color = color.bars, # Farbe Balken
                         xlab = "", # Beschriftung x-Achse
                         main = "", # Titel der Abbildung
                         cutoff = FALSE) # cutoff, bei z.B. 12 werden alle Werte über 12 als "12+" abgebildet
{
  
  if(cutoff != FALSE) {
    xax <- ifelse(max(x, na.rm = TRUE) == cutoff, "n", "t")
    labs <- as.character(as.data.frame(table(x))[, 1])
    labs[length(labs)] <- paste0(cutoff, "+")
  } else {
    xax <- "t"
  }
  
  bp <- plot(descr::freq(x, plot = FALSE), 
             col = color.bars, 
             family = font.family, 
             ylim = c(0, 5/4*max(table(x))),
             xlab = xlab,
             main = main, 
             ylab = "Häufigkeit",
             xaxt = xax,
             border = "gray50",
             cex.lab = 1,
             font.lab = 2,
             col.lab = "gray30")
  
  if(cutoff != FALSE){
    if (max(x, na.rm = TRUE) == cutoff){ axis(1, 
                                              at = bp[, 1], 
                                              labels = labs, tick = FALSE,
                                              family = font.family)}}
}



# Horizontaler Barplot für Abbildungen von SC/MC-Fragen
barplot.sc.mc <- function(x, # Daten (data.frame mit Fragetexten, Häufigkeit und Prozent)
                          color = color.bars, # Farbe der Balken
                          xlab = "") # Beschriftung x-Achse
{ 
  
  cody <- x
  
  #wenn alles Nullen(Daten gleich Nullvektor), dann schreibe "*Grafik wurde wegen ...
  if(all(cody[, 2] == 0)){ cat("*Grafik wurde wegen fehlender Daten nicht erstellt.*  \n  \n")}
  else{
    cody <- cody[cody$freq > 0, ]
    cody$label <- factor(cody$label, levels = rev(cody$label)) #Reihenfolge der Label festlegen
    
    ggplot(data=cody, aes(x=label, y=freq)) + #auf x-Achse wird Kategorie/Label dargestellt, auf y-Achse Haeufigkeiten
      #geom_hline(yintercept = v, color = "grey70")+
      theme(panel.background = element_rect(fill = 'white', colour = 'gray70'), #Hintergrund wird auf wei? gestellt
            panel.grid.major.x = element_line(color = "grey70")) + #Hilfslinien 
      theme(text=element_text(family = font.family)) +
      scale_color_manual(values = "grey50") +
      geom_bar(stat="identity", #Balkendiagramm
               fill = color, #Farbe
               width=0.8)+ #Breite der Balken
      geom_col(fill = color, color = "black", size = 0.3) +
      geom_text(aes(label = paste(perc, "%", sep = "")), #Prozentzahl über die Balken schreiben
                color ="grey30",#Farbe 
                size = 3.5, #Schriftgroesse
                family = font.family,
                hjust = -0.5)+ #horizentale Verschiebung des Textes
      ylim(c(0, DescTools::RoundTo((max(x$freq)*1.15), multiple=DescTools::RoundTo(max(x$freq) * 0.3, multiple=5, ceiling), ceiling))) +#automatische Einstellung des Limites der y Achse
      theme(title = element_text(color = "blue"),
            axis.title.y = element_blank(),  #y-Achsen Titel, element_blank() meint kein Text
            axis.title.x = element_text(color = "grey30", size = 14, face = "bold"),  #x Achsen Titel
            axis.text.y = element_text(color = "grey40", #Beschriftung y Achse, element_text() meint schreibe einen Text, col = Farbe
                                       size = 12, face = "bold"), #Schriftgroesse
            axis.text.x = element_text(color = "black", size = 11), #Beschriftung x Achse
            axis.ticks.y = element_blank(), #Die kleinen Striche zur Unterteilung an der y Achse
            axis.ticks.x = element_line(color = "grey48"))+ #kleinen Striche zur Unterteilung an der x Achse
      labs(y=xlab)+
      coord_flip()+ #x- und y-Achse vertauschen
      theme(text=element_text(family = font.family)) 
  }
}



#### BOXPLOTS ####


# Boxplot für Rücklaufsabbildung (LVE)
boxplot.ruecklauf <- function(x, # Objekt mit Teilnehmendenzahlen
                              kennung) #  Kennungen
{
  z <- data.frame(kennung, x) # Erstelle einen Datensatz aus beiden
  z.uni <- z[!duplicated(z$kennung),] # Nehme nur eine Zeile pro Kennung
  tb.kennung <- data.frame(table(kennung)) # Zähle, wie oft jede Kennung vorkommt 
  all <- merge(z.uni, tb.kennung, by = "kennung") # Füge das mit dem Datensatz z zusammen
  
  # Datensatz z enthält an dieser Stelle:
  # Eine Spalte mit jeder LV-Kennung
  # Eine Spalte mit der zugelassenen Teilnehmendenzahl pro Kennung
  # Eine Spalte mit der bisherigen Teilnehmendenzahl 
  
  x.new <- as.numeric(all$Freq)/as.numeric(all$x)*100 # Teile die bisherigende Tn-Zahl durch die Zugelassenen (mal hundert)
  x <- x.new
  
  
  
  # Ausgabe der Flextable
#  print(table.stat.single(x, col1.name = "N\\textsubscript{courses}")) # kein print() bei Flextable
#  table.stat.single(x, col1.name = "N_courses") # klappt nicht
#  knitr::knit_print(table.stat.single(x, col1.name = "N_courses")) # klappt nicht
#  flextable_to_rmd(table.stat.single(x, col1.name = "N_courses")) # für if und for bei Quarto erforderliche Funktion
  flextable_to_rmd( # Ausgabe der Flextable
    table.stat.single(x, col1.name = "N") %>%
      append_chunks(as_sub("courses"), i=1, j=1, part="header") # courses tiefergestellt
    )
  
  cat("  \n  \n")
  
  #Grafikparameter spezifizieren
  opar <- par(no.readonly = TRUE)
  par(mar=c(2.1, 5 , 0.1, 2.5)) # 2. Zahl anpassen, wenn Aenderung der Breite gewuenscht (b l t r )
  par(fg="gray80")
  #Boxplot zeichnen
  boxplot(x, width=NULL, outline=TRUE, 
          boxwex=0.5,   #boxwex stellt die Groesse der Box vom boxplot ein
          horizontal=TRUE, #Orientierung: horizontal
          col=color.bars, #Fuellfarbe
          ylim=c(0,120), #Laenge der y Achse
          xaxt="n", #keine Beschriftung der x Achse
          border="black", #Farbe der Aussenlinie 
          pars=list(outcol=color.bars, outpch=20))#Spezifizierung fuer Ausreisser. Farbe & Art/Charakter
  abline(v=c(0,20,40,60,80,100)) #vertikale Hilfslinien
  #Boxplot wird ?ber Hilfslinie gezeichnet (Befehl von oben wiederholt)
  boxplot(x, width=NULL, outline=TRUE, boxwex=0.5,   
          horizontal=TRUE, col=color.bars, ylim=c(0,120), xaxt="n", 
          border="black", pars=list(outcol= color.bars, outpch=20), add = TRUE)
  mtext(c("Rücklauf \nin Prozent"), #Text
        side=2, #on which side of the plot (1=bottom, 2=left, 3=top, 4=right).
        line=1, #Linienart (durchgezogene Linie)
        at=1, #give location of each string in user coordinates
        las=1, #Schriftrotierung
        # las
        # numeric in {0,1,2,3}; the style of axis labels.
        # 0: always parallel to the axis [default],
        # 1: always horizontal,
        # 2: always perpendicular to the axis,
        # 3: always vertical.
        col="gray50", #Farbe der Schrift
        cex=0.8,
        font = 2) #cex = Schriftgroesse
  mtext(c("0", "20", "40", "60", "80", "100"), side=1, line=1, at=c(0, 20, 40, 60, 80, 100), las=1, col="gray50", cex=0.8)
  axis(side=1, at=c(0, 20, 40, 60, 80, 100), labels=FALSE)
  
  par(opar)
}

# Boxplots für Skalenfragen auf aggregiertem Niveau: Funktioniert, sollte aber überarbeitet werden
boxplot.aggr.sk <- function(x, p, q, d, e, color = color.bars)
{ #x = daten, p = labels/Text/Beschriftungen Y Achse, q = Skala x Achse, d = Anzahl von p, e = Anzahl von q
  daten <- cbind(x)
  opar <- par(no.readonly = TRUE)
  par(mar=c(2.1, 21.5, 0.1, 2.1)) 
  if(e == 5) {par(mar=c(4, 21.5, 0.1, 2.1))}
  par(family = font.family)
  boxplot(daten, xaxt="n", yaxt="n", #x- und y-Achsentext nicht anzeigen
          ylim=c(1,e), #Länge y Achse von 1 bis e
          horizontal=TRUE, #horizontale Ausrichtung
          boxwex=0.8, #stellt Groesse der Box des Boxplots ein
          border = "white")
  abline(v=c(1:e), col="gray80")#vertikale Hilfslinien 
  par(new=TRUE)
  par(fg="gray80")
  par(family = font.family)
  boxplot(daten, xaxt="n", yaxt="n", ylim=c(1,e), horizontal=TRUE, 
          boxwex=0.8, col= color.bars, border="black", 
          pars=list(outcol= color.bars, outpch=20))
  mtext(p,
        side=2, line=1, at=1:d, las=2, 
        col="gray30", #Beschriftung Farbe
        cex = 1) #Beschriftung Schriftgroesse
  mtext(q,
        side=1, line=1, at=1:e, las=1, col="gray30", cex=1, font = 2)
  if (e == 5) {mtext("Hinweis: andere Skalenlogik (im Vergleich zu den 6er-Skalen)", side=1, line=3, col="gray40", font = 3)}
  par(opar)
}

# Abbildung der Gesamtnote
boxplot.gesnote <- function(x) # Daten
{
  #Grafikparameter bestimmen
  opar <- par(no.readonly = TRUE)
  par( mar=c(2.1, 7 , 0.1, 2.1)) # 2. Zahl anpassen, wenn Aenderung der Breite gewünscht
  par(fg="gray80")
  par(family = font.family)
  #Boxplot zeichnen
  boxplot(x, width=NULL, outline=TRUE, 
          boxwex=0.5,   #boxwex stellt die Groesse der Box vom boxplot ein
          horizontal=TRUE, #Orientierung: horizontal
          col= color.bars,  #Fuellfarbe
          ylim=c(1,6), #Laenge der y Achse
          xaxt="n", #keine Beschriftung der x Achse
          border="black", #Farbe der Aussenlinie 
          pars=list(outcol= color.bars, outpch=20))#Spezifizierung fuer Ausreisser. "outpch=NA" > ohne Ausreiser
  abline(v=c(1,2,3,4,5,6), col="gray80") #Hilfslinien
  boxplot(x, width=NULL, outline=TRUE, boxwex=0.5,   
          horizontal=TRUE, col= color.bars, ylim=c(1,6), xaxt="n", border="black", 
          pars=list(outcol= color.bars, outpch=20), add = TRUE)
  
  mtext(c("Gesamtnote \nder LV"), 
        side=2, # on which side of the plot (1=bottom, 2=left, 3=top, 4=right).
        line=1, #Linienart (durchgezogene Linie)
        at=1, #an x-Koordinate 1
        las=1, # Textausrichtung
        col="gray30",#Textfarbe 
        cex = 1.1,
        font = 2)#Schriftgroesse
  mtext(c("sehr gut", "gut", "befriedigend", "ausreichend", "mangelhaft", "ungenügend"),
        side=1, line=1, at=c(1, 2, 3, 4, 5, 6), las=1, col="gray50", cex=1.1, font = 2)
  axis(side=1, at=c(1, 2, 3, 4, 5, 6), labels=FALSE)
  
  par(opar)
  
}

# Boxplot mit workloads der LVs: Funktioniert, sollte überarbeitet werden
boxplot.workload <- function(x,p,q,d,e, nums) # x = daten, p = Text/Beschriftungen Y Achse, q = Skala x Achse, d = Anzahl von p, e = Anzahl von q
{ 
  opar <- par(no.readonly = TRUE)
  par(fg="gray80")
  par(family = font.family)
  #par(mar=c(7, 4.1, 4.1, 4.2))
  par(mar=c(7, 4.1, 2, 4.2))
  boxplot(x, col= color.bars, border="black", ylab=NULL, xlab=NULL, horizontal = TRUE, ylim=c(0,e),
          xaxt="n", yaxt="n", boxwex = 0.8, pars=list(outcol= color.bars, outpch=20))
  abline(v = c(0:e))
  boxplot(x, col= color.bars, border="black", ylab=NULL, xlab=NULL, horizontal = TRUE, ylim=c(0,e),
          xaxt="n", yaxt="n", boxwex = 0.8, pars=list(outcol= color.bars, outpch=20), add = TRUE)
  axis(side=2, at=1:d, labels=FALSE)
  mtext(p, side=2, line=1, at=1:d, las=1, col="black", cex=0.8)
  axis(side=4, at=1:d, labels=FALSE, tick = FALSE)
  mtext(nums, side=4, line=1, at=1:d, las=1, col= "black", cex=0.8)
  axis(side=1, at=0:e, labels=FALSE)
  mtext(q,side=1, line=1, at=0:e, las=1, col="black", cex=0.8)
  mtext("ECTS der LV", side=2, line=3, col="gray30", font = 2)
  mtext("Anzahl Veranstaltungen", side=4, line=3, col= "gray30", font = 2)
  mtext("angegebener Workload getrennt nach ECTS der LV", side=1, line=3, col="gray30", font = 2)   # geändert: line 2 > line 3
  mtext("[ECTS entstammen Angaben aus LVE-Anmeldung (*.csv); \nfalls nicht angegeben = 'k.A.']", side=1, line=6, col="gray40")
  par(opar)
}


#
boxplot.vert <- function(x,
                         kennung,
                         dont.know = "") #gibt es eine "weiß nicht"-Option?
{
  if (dont.know == "") {
    AnzahlOptionen <- length(unique(na.omit(x)))
    if (AnzahlOptionen == 3) {dont.know <- TRUE
    } else if (AnzahlOptionen == 2) {dont.know <- FALSE
    } else { stop("Aus diese Daten lässt sich kein passender Boxplott erzeugen,
           wahrscheinlich weil es nicht um eine 'ja/nein Frage' handelt.
          Es könnte aber auch sein, dass zu dieser Frage bei der ausgewählten
            Untergruppe keine Antworten existieren.") }}
  
  x.ja <- x
  x.ja[x.ja != 1] <- NA
  
  x.nein <- x
  x.nein[x.nein != 2] <- NA
  
  if (dont.know == TRUE) {
    x.wn <- x
    x.wn[x.wn != 66] <- NA}
  
  if (dont.know == TRUE) {x.df <- data.frame(x.ja, x.nein, x.wn)
  } else {x.df <- data.frame(x.ja, x.nein)}
  
  x.aggr <- data.frame(x.df[0, ])
  
  if (dont.know == TRUE) { 
    for (n in unique(kennung)) {
      tmp.sub <- as.data.frame(x.df[kennung == n, ])
      x.aggr[nrow(x.aggr)+1, 1:3] <- c(sum(!is.na(tmp.sub[, 1])) / nrow(tmp.sub) * 100, 
                                       sum(!is.na(tmp.sub[, 2])) / nrow(tmp.sub) * 100, 
                                       sum(!is.na(tmp.sub[, 3])) / nrow(tmp.sub) * 100)
    }} else {
      for (n in unique(kennung)) {
        tmp.sub <- as.data.frame(x.df[kennung == n, ])
        x.aggr[nrow(x.aggr)+1, 1:2] <- c(sum(!is.na(tmp.sub[, 1])) / nrow(tmp.sub) * 100, 
                                         sum(!is.na(tmp.sub[, 2])) / nrow(tmp.sub) * 100)
      }}
  
  
  if (dont.know == TRUE) {colnames(x.aggr) <- c("ja", "nein", "weiß nicht")
  } else {colnames(x.aggr) <- c("ja", "nein")}
  
  cat(paste0("\\subsubsection{ ", Hmisc::label(x), "}  \n  \n"))
  par(family = "Raleway")  
  par(fg="gray80")
  par(mar=c(8, 4.1, 4.1, 2.1)) # 2. Zahl anpassen, wenn Änderung der Breite
  # gewünscht (unten links oben rechts)
  boxplot(x.aggr,
          col = color.bars,
          xaxt = "n",
          boxwex = 0.6,
          pars=list(outcol= color.bars, outpch=20), border = "black",
          ylim = c(0, 100))
  abline(h = c(20,40,60,80), col = "gray80")
  par(new = TRUE)
  boxplot(x.aggr,
          col = color.bars,
          xaxt = "n",
          boxwex = 0.6,
          pars=list(outcol= color.bars, outpch=20), add = TRUE, border = "black",
          ylim = c(0, 100))
  
  title(ylab = "Häufigkeit in Prozent pro LV", xlab = "Antwortoption",
        font.lab = 2, col.lab = "gray30")
  
  if (dont.know == TRUE) {axis(side = 1, tick = "FALSE", labels = colnames(x.aggr),
                               at = c(1, 2, 3))} else {axis(side = 1, tick = "FALSE", labels = colnames(x.aggr),
                                                            at = c(1, 2))}
  
  mtext("Häufigkeitsverteilung der prozentualen Anteile der
möglichen Antwortoptionen pro LV.",
        side=1, line=6, col="gray40")
}

#### EVASYS-PLOTS ####


# Barplot-Boxplot Hypbrid für die Darstellung von ordinalskalierten Variablen
#optimale chunk-einstellung: fig.width=6, fig.height=1.4
evasys.skala.plot <- function(x, # Daten
                              tmin, # Beschriftung links
                              tmax, # Beschriftung rechts
                              number = 6) # Skala (6 für Sechserskala etc.)
{ 
  x[!(x %in% c(1:number))] <- NA
  x <- x[!is.na(x)]
  
  
  tmin <- auto.newline2(tmin, number = 15)
  tmax <- auto.newline2(tmax, number = 15)
  
  line.tmin <- ifelse(grepl("\\n", tmin), -1.7, -2)
  line.tmax <- ifelse(grepl("\\n", tmax), -1.7, -2)
  
  bobby <- x %>% 
    psych::describe(.) %>% 
    round(.,2) %>% 
    data.frame() %>%
    dplyr::select(n, mean, sd, min, max) %>% 
    data.frame()
  
  xtab <- table(c(x, 1:number)) - 1 #damit alle angezeigt werden
  
  par(mar=c(2, 5.3, 2.1, 5.3))
  par(fg="gray50") # Farbe Rand 
  par(family = font.family)
  barplot(rep(NA, number),ylim=c(0,sum(table(x))),axes=FALSE)
  abline(v=c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7), col = "grey80")
  bp <- barplot(xtab, #damit alle angezeigt werden
                ylim=c(0, sum(table(x))), 
                col = color.bars,
                axes = FALSE, add = TRUE, axisnames = FALSE)
  box()
  axis(side = 1, at=bp, tick = FALSE, labels = c(1:number), 
       cex.axis = 0.65, line = -0.7)
  
  axis(side = 3, at=bp, tick = FALSE, labels = paste(round(100*prop.table(xtab),1), " %", sep=""), 
       cex.axis = 0.65, line = -0.5)
  par(new=TRUE)
  bxp <- boxplot(as.numeric(x), plot=FALSE)
  bxp$stats <- matrix(c((bobby$mean-bobby$sd), bobby$mean, bobby$mean, bobby$mean, (bobby$mean+bobby$sd)))
  invisible(ifelse(bxp$stats[5,1]>number, bxp$stats[5,1] <- number, bxp$stats[5,1] <- bxp$stats[5,1]))
  invisible(ifelse(bxp$stats[1,1]<1, bxp$stats[1,1] <- 1, bxp$stats[1,1] <- bxp$stats[1,1]))
  bxp(bxp, horizontal = TRUE, ylim=c(0.6,number + 0.4), xlim = c(0.3,1.3), boxcol = rgb(0.55, 0, 0), staplewex = 0.6, staplelwd=2,
      boxlwd=3, 
      whisklty = 1, whisklwd=2, outline = FALSE, axes = FALSE)
  mtext(tmin, side=1, at = -0.5, line = line.tmin, font = 2, col = "gray30", cex = 0.65)
  mtext(tmax, side=1, at = number*1.27, line = line.tmax, font = 2, col = "gray30", cex = 0.65)
  
  cat("  \n  \n")
}


#### MERGE FUNKTIONEN ####


### merge-Funktionen kombinieren mehrere Funktionen als Bausteine und ermöglichen, über eine Funktion direkt
### eine Tabelle und eine Graphik zu erhalten. 

# merge-Funktion für Fachsemester
merge.fachsem <- function(x, # Daten
                          fig.height = 5, # figure-height des Plots im Markdown, 5 ist optimal bei cutoff 12, damit Tabelle und Abbildung auf eine Seite passen
                          cutoff = 12, # cutoff-Wert, alle Werte >= cutoff werden zusammengefasst
                          group = "a", # Gruppe: "a" für alle, "b" für Bachelor und "m" für Master
                          inkl = "nr", # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                          nr = "") # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
{
  if (inkl == "nr") {
    if (nr == "") {inkl <- TRUE} else {inkl <- eval(parse(text = paste0("inkl.", nr)))}
  }
  
  if (inkl == TRUE) {
    
    if(group == "a"){
      caps <- "(alle)"
      xl <- "Fachsemester alle"
    }
    
    if(group == "b"){
      caps <- "(nur Bachelor)"
      xl <- "Fachsemester Bachelor"
    }
    
    if(group == "m"){
      caps <- "(nur Master)"
      xl <- "Fachsemester Master"
    }
    
    
    x[x >= cutoff] <- cutoff
    
    
    cat(paste0("## Fachsemester ", caps, "  \n  \n"))
    cat("\\subsubsection{ Bezogen auf das Fach, dem die vorliegende Veranstaltung zugehoert: in welchem Fachsemester sind Sie eingeschrieben?}  \n  \n")
    
    # kein print bei Flextable
    #print(table.freq(x, col1.name = xl, cutoff = cutoff)) # main ist die Überschrift
    #table.freq(x, col1.name = xl, cutoff = cutoff)
    
    flextable_to_rmd( # Ausgabe der Flextable
      table.freq(x, col1.name = xl, cutoff = cutoff) %>%
        append_chunks(as_sub("votes"), i=1, j=2, part="header") # votes tiefergestellt
    )
    
    
    
    cat("  \n  \n")
    
    subchunkify(barplot.freq(x, xlab = xl, cutoff = cutoff), fig_height = 5, fig_width = 10) # xlab ist Label x-Achse
    
    cat("  \n  \n")
    
  }
}

# merge-Funktion für mehrere Skalenfragen, die auf einmal dargestellt werden sollen
merge.multi.sk <- function(x, # Daten
                           kennung, # Objekt mit Kennungen (oder Fallnummern, nur bei Aggregierung benötigt
                           number = "default", # Skala: 6 für Sechser, etc. (OHNE AUSWEICHOPTION)
                           alt1 = FALSE, # Text für erste Ausweichoption (standardmäßig 0 in den Daten, siehe alt1.num)
                           alt2 = FALSE, # Text für zweite Ausweichoption (standardmäßig 7 in den Daten, siehe alt1.num)
                           alt1.num = 0, # Welche Zahl entspricht alt1
                           alt2.num = 7, # Welche Zahl entspricht alt2
                           nr = "", # Nummer der ersten Frage
                           inkl = "nr", # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                           tmin = "default", # linker Pol, bei "default" wird das Label automatisch gezogen
                           tmid = "default", # mittlerer Pol (für 5er Skalen), bei "default" automatisch
                           tmax = "default",  # rechter Pol, "default" wie oben
                           show.table = TRUE, # Soll Tabelle angezeigt werden?
                           show.plot = TRUE, # Sollen Boxplots dazu angezeigt werden?
                           fig.height = "default", # Höhe der Abbildung, bei "default" ist es Anzahl der Fragen + 1
                           col2.name = "n", # Titel der n-Spalte, in LVE in "N\\textsubscript{courses}" ändern
                           message = "", # Soll ein Hinweistext am Anfang erfolgen?
                           aggr = FALSE) # Sollen Daten aggregiert werden?
{
  # x = Variablen, number = 5 oder 6 (5er, Sechser oder Siebener-Skala)
  # scale = Skalenbeschreibung
  
  if (inkl == "nr") {
    
    if (nr == "") {inkl <- TRUE} else {
      
      header <- sub("\\..*$", "", nr)
      nr1 <- as.numeric(sub("^.*\\.", "", nr))
      nr.end <- nr1 + ncol(x) - 1
      nrs.ends <- nr1:nr.end
      nrs <- paste0(header, ".", nrs.ends)
      
      inkls <- NULL
      for (k in 1:length(nrs)) {inkls[k] <- eval(parse(text = paste0("inkl.", nrs[k])))}
      
      x <- x[, inkls] # Variablen entfernen, die nicht vorkommen sollen
      nrs <- nrs[inkls] # Nummern entfernen, die nicht vorkommen sollen
      
      if (length(nrs > 0)) {
        
        for (k in 1:length(nrs)) {
          attr(x[, k], "label") <- paste0("\\textbf{", nrs[k], "} ", attr(x[, k], "label"))
        }
      }
      
      inkl <- ifelse(any(inkls == TRUE), TRUE, FALSE)
      
      rm(header, nr1, nr.end, nrs.ends, nrs, inkls)
      
    }
  }
  
  if (inkl == TRUE) {
    
    if (number == "default") { # zieht sich automatisch die Anzahl der Stufen
      # Items, falls diese nicht angegeben wurde
      
      StufenListe <- list()
      if (!is.null(ncol(x))) {
        for (k in 1:ncol(x)) {
          StufenListe[k] <- length(attr(x[,k], "labels"))
        }
        Stufen <- unique(StufenListe)
        if (length(Stufen) != 1) { # Fehlermeldung bei unterschiedlicher Anzahl Stufen
          stop("Die ausgewählten Items haben eine unterschiedliche
             Anzahl an Stufen.")} else {
               number <- Stufen[[1]]}
      } else {number <- length(attr(x, "labels"))}
    }
    x <- data.frame(x)
    
    if (ncol(x) > 1) {
      labels <- as.character(lapply(x, attr, which = "label"))
    } else {
      labels <- attr(x[, 1], "label")
    }
    
    
    ListeLabels <- list()
    for (k in 1:length(x)) {
      ListeLabels[[k]] <- names((attr(x[, k], "labels")))[1:number]  #nicht Relevante Labels werden abgeschnitten
    }
    
    TabelleLabels <- as.data.frame(ListeLabels, col.names = 1:ncol(x))
    
    if (tmin == "default") {
      LabelLinks <- unique(as.list(TabelleLabels[1,]))
      
      if(length(unique(LabelLinks)) != 1) {
        warning(paste0("Achtung: Die Labels der einzelnen Items auf der linken
                     Seite sind unterschiedlich: \n", LabelLinks))}
      
      tmin <- LabelLinks[[1]]}
    
    if (tmid == "default" & number %% 2 == 1) { #nur bei ungerader Anzahl Stufen
      
      LabelMitte <- unique(as.list(TabelleLabels[(number+1)/2,]))
      
      if(length(unique(LabelMitte)) != 1) {
        warning(paste0("Achtung: Die Labels der einzelnen Items in der Mitte
                     sind unterschiedlich: \n", LabelMitte))}
      
      tmid <- LabelMitte[[1]]}
    
    if (tmax == "default") {
      
      LabelRechts <- unique(as.list(TabelleLabels[number,]))
      
      if(length(unique(LabelRechts)) != 1) {
        warning(paste0("Achtung: Die Labels der einzelnen Items auf der rechten
                     Seite sind unterschiedlich: \n", LabelRechts))}
      
      tmax <- LabelRechts[[1]]}
    
    
    if (aggr == TRUE) {
      x <- aggr.data(vars = x, kennung = kennung)
    }
    
    if (number %% 2 == 0 | tmid == "") { #bei gerader Anzahl Stufen oder keinem Mittellabel
      text.skala <- paste0("(1) ", tmin, " - (", number, ") ", tmax)
      labels.skala <- c(tmin, rep("", number-2), tmax)
    } else { #bei ungerader Anzahl Stufen 
      text.skala <- paste0("(1) ", tmin, " - (", (number+1)/2, ") ", tmid,
                           " - (", number, ") ", tmax)
      labels.skala <- c(tmin, rep("", (number-3)/2), tmid,
                        rep("", (number-3)/2), tmax)}
    
    if(message != "") {cat(message)}
    
    
    # Alternativantworten in Listen schreiben (für die Tabelle)
    if (alt1 != FALSE) {
      
      alt1.list <- NULL
      for (l in 1:ncol(x)) {
        alt1.list <- c(alt1.list, sum(x[, l] == alt1.num, na.rm = TRUE))
      }
    }
    
    if (alt2 != FALSE) {
      
      alt2.list <- NULL
      for (l in 1:ncol(x)) {
        alt2.list <- c(alt2.list, sum(x[, l] == alt2.num, na.rm = TRUE))
      }
    }
    
    x[x < 1 | x > number] <- NA
    
    if (show.table == TRUE) {
    flextable_to_rmd(
#      subchunkify(
#        flextable_to_rmd(
        table.stat.multi(
          x,
#          col1.name = paste0("\\textbf{Item} \\textit{[Skala: ", text.skala, "]}"),
#          col1.name = paste0("Item [Skala: ", text.skala, "]"),
          col1.name = "Item",
          col2.name = col2.name,
          bold.col1 = FALSE,
          alt1 = alt1,
          alt2 = alt2,
          alt1.list = alt1.list,
          alt2.list = alt2.list
        ) %>%
        bold(i=1, j=1, part="header", bold=FALSE) %>%
        mk_par(
          i=1, j=1, part="header",
          as_paragraph(
            as_b("Item"),
            colorize(as_i(paste0(" [Skala: ", text.skala, "]")), color="gray20")
          )
        )

#        append_chunks(
#          i=1,
#          j=1,
#          part="header",
#            colorize(as_i(paste0(" [Skala: ", text.skala, "]")), color="black")
#        )
          
          
#        fig_height = 7,
#        fig_width = 9
#      )
    )
    }
    
    if (show.plot == TRUE) {
      labels <- rev(labels)
      x <- rev(x)
      labels <- auto.newline(labels)
      
      if (fig.height == "default")
      {
        subchunkify(
          boxplot.aggr.sk(x, labels, labels.skala, length(labels), number),
          fig_height = (length(labels) + 1),
          fig_width = 9
        )
      }
      else
      {
        subchunkify(
          boxplot.aggr.sk(x, labels, labels.skala, length(labels), number),
          fig_height = fig.height,
          fig_width = 9
        )
      }
      
    }
    
    cat("  \n  \n")
  }
}


# merge-Funktion zum Zusammenfügen der single-choice Fragen nach dem 1. und 2. Fach
merge.subj <- function(x1, # Daten von Fach 1
                       x2, # Daten von Fach 2
                       inkl1 = "nr1", # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr1" zieht sich automatisch die entsprechende inkl. Variable
                       inkl2 = "nr2", # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr2" zieht sich automatisch die entsprechende inkl. Variable
                       nr1 = "", # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
                       nr2 = "") # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
{
  if (inkl1 == "nr1") {
    if (nr1 == "") {inkl1 <- TRUE} else {inkl1 <- eval(parse(text = paste0("inkl.", nr1)))}
  }
  
  if (inkl2 == "nr2") {
    if (nr2 == "") {inkl2 <- TRUE} else {inkl2 <- eval(parse(text = paste0("inkl.", nr2)))}
  }
  
  if (inkl1 == TRUE && inkl2 == TRUE) {
    
    #    subj1 <- data.frame(fach = unlist(x1, use.names = FALSE))
    #    subj2 <- data.frame(fach = unlist(x2, use.names = FALSE))
    
    subj <- rbind(data.frame(fach = unlist(x1, use.names = FALSE)), 
                  data.frame(fach = unlist(x2, use.names = FALSE))) # Zusammenfügen der beiden Fächer-Spalten
    
    attr(subj$fach, "label") <- paste0(" & ", nr2, " ",
                                       sub("\\?.*", "", attr(subj$fach, "label")),
                                       " / 2. Fach? ")  # Vergabe des neuen Labels
    
    merge.sc(subj$fach, nr = nr1) # Aufruf der merge.sc-Funktion
    
    cat("\\textit{Hinweis: In der Befragung wurden 1. und 2. Fach getrennt abgefragt; in dieser Tabelle werden die Antworten gemeinsam dargestellt. Daraus ergibt sich in dieser Darstellung eine Verdopplung des Stichprobenumfangs (siehe \"Total\").}  \n  \n")
    
  }
}



# merge-Funktion für single-choice Fragen
merge.sc <- function(x, # Daten
                     inkl = "nr", # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                     nr = "", # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
                     fig.height = "default", # Höhe der Abbildung, bei "default" ist es Anzahl der Fragen*0.75 +1
                     already.labels = FALSE, # Wurden die Daten bereits in Label umgewandelt?
                     col2.name = "n", # Name der n-Spalte in Tabelle
                     order.table = FALSE, # Soll nach Häufigkeit sortiert werden? "decreasing" für absteigendes Sortieren
                     show.plot = show.plot.sc, # Soll der Plot angezeigt werden?
                     no.pagebreak = TRUE) # Seitenumbrüche mittendrin verhindern?
{
  if (sum(!is.na(x)) > 0) {
    
    if(already.labels == FALSE) {x <- sjlabelled::to_label(x)}
    
    if (inkl == "nr") {
      if (nr == "") {inkl <- TRUE} else {inkl <- eval(parse(text = paste0("inkl.", nr)))}
    }
    
    if (inkl == TRUE) {
      if(no.pagebreak == TRUE) #{cat("\\begin{minipage}{\\linewidth} \n")} # funktioniert nicht in Quarto
      {cat("\\pagebreak  \n  \n")}
      cat("\\subsubsection{ " , nr, " ", replace.latex.issues(attr(x, "label")), "}  \n  \n")
      
      
      
#      print(table.freq(x, col1.name = "Antwortoption", col2.name = col2.name, # kein Print bei Flextable
#                       order.table = order.table))
      flextable_to_rmd(table.freq(x, col1.name = "Antwortoption", col2.name = col2.name, 
                                   order.table = order.table))
      
      freq.tab <- freq(x, plot = FALSE)
      results <- data.frame(rownames(freq.tab), round(freq.tab[, 1:2], digits = 2))
      results[, 1] <- as.character(auto.newline2(results[, 1], number = 40))
      results <- results[!(rownames(results) %in% c("NA's", "Total")), ]
      colnames(results) <- c("label", "freq", "perc")
      
      
      par(family = "Raleway")
      
      if(show.plot == TRUE) {
        if(fig.height == "default") 
        {subchunkify(barplot.sc.mc(results, xlab = "Häufigkeit"), fig_height = (1 + 0.75*nrow(results)), fig_width = 9)}
        else 
        {subchunkify(barplot.sc.mc(x = results, xlab = "Häufigkeit"), fig_height = fig.height, fig_width = 9)}
      }
      if(no.pagebreak == TRUE) {
        cat("\n\\bigskip")
        #cat("\n\\end{minipage}") # funktioniert nicht in Quarto
      }
      cat("   \n  \n")
      
    }
  }
}



# merge-Funktion für MC-Fragen
merge.mc <- function(x, # Daten (dataframe mit mehreren Spalten) -> Wichtig: Darauf achten, das Labels enthalten sind
                     head = "default", # Fragetext, bei "default wird dieser automatisch aus den Lables gezogen
                     col1.name = "Antwortoption", # Erste Zelle der ersten Spalte in Tabelle
                     col2.name = "n", # Name der n-Spalte in Tabelle
                     show.table = TRUE, # Soll Tabelle angezeigt werden?
                     fig.height = "default", # Höhe der Abbildung, bei "default" ist es Anzahl der Antwortoptionen*0.75 +1
                     inkl = "nr", # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                     nr = "", # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
                     lime = FALSE, # Für Daten im Format nach LimeSurvey Export (nach Syntax-Skript)
                     filter = FALSE, # FILTER-Klammer für LimeSurvey
                     valid.perc = TRUE, # mit gültigen Prozent?
                     order.table = FALSE, # Soll nach Häufigkeit sortiert werden? "decreasing" für absteigendes Sortieren
                     show.plot = show.plot.mc) # Soll der Plot angezeigt werden?
{
  if (inkl == "nr") {
    if (nr == "") {inkl <- TRUE} else {inkl <- eval(parse(text = paste0("inkl.", nr)))}
  }
  
  if (inkl == TRUE) {
    
    if (lime == TRUE) {
      
      copy <- x
      
      for (l in 1:ncol(x)) {
        
        label <- attr(x[, l], "label")
        answer <- sub("\\].*", "", label)
        answer <- sub("\\[", "", answer)
        label_cut <- sub(".*].", "", label)
        
        
        x[, l] <- as.numeric(x[, l], na.rm = TRUE)
        x[x[, l] == 2 & !is.na(x[, l]), l] <- 0
        x[x[, l] == 1 & !is.na(x[, l]), l] <- l
        
        
        attr(x[, l], "label") <- paste0(label_cut, " (Mehrfachantwort möglich): ", answer)
      }
      
      if (filter != FALSE) {
        attr(x[, 1], "label") <- paste0("[", filter, "] ", attr(x[, 1], "label"))
      }
      
      
    }
    
    
    if (head == "default") {head <- sub(":.*", "", attr(x[, 1], "label"))}
    cat("\\subsubsection{", nr, replace.latex.issues(head), "}  \n  \n")
    
    val.labels <- sub('.*: ', '', as.character(lapply(x, attr, which = "label")))
    
    
    if (valid.perc == TRUE) {
      
      results <- data.frame(matrix(nrow = length(x), ncol = 4))
      colnames(results) <-
        c(col1.name, col2.name, "\\%", "gültige \\%")
      results[, 1] <- replace.latex.issues(val.labels)
      for (n in 1:length(x)) {
        results[n, 2] <- sum(x[, n] != 0, na.rm = TRUE)
        results[n, 3] <- round(results[n, 2] / nrow(x) * 100, digits = 2)
        results[n, 4] <-
          round(results[n, 2] / nrow(x[!is.na(x[, 1]),]) * 100, digits = 2) 
      }
      
      if (order.table != FALSE) {
        
        decreasing <- ifelse(order.table == "decreasing", TRUE, FALSE)
        results <- results[order(results[, 2], decreasing = decreasing), ]
        
      }
      
      results[nrow(results)+1, ] <- c("NAs", nrow(x[is.na(x[, 1]),]),
                                      round(nrow(x[is.na(x[, 1]),]) / nrow(x) * 100, 2),
                                      "NA")
      results[nrow(results)+1, ] <- c("Total", nrow(x),
                                      "NA",
                                      "NA")
      
    } else {
      results <- data.frame(matrix(nrow = length(x), ncol = 3))
      colnames(results) <-
        c(col1.name, "N_votes", "\\%")
      results[, 1] <- replace.latex.issues(val.labels)
      for (n in 1:length(x)) {
        results[n, 2] <- sum(x[, n] != 0, na.rm = TRUE)
        results[n, 3] <-
          round(results[n, 2] / nrow(x) * 100, digits = 2)
      }
      
    }
    
    if(ncol(results) == 4) {col.width <- col.width4} else {col.width <- col.width3}
    
    if(show.table == TRUE) {subchunkify(lv.kable(results, col.width = col.width) , fig_height = 7, fig_width = 9)}
    
    colnames(results) <- c("label", "freq", "perc")
    results[, 1] <- as.character(auto.newline2(results[, 1], number = 40))
    
    if(show.plot == TRUE) {
      if(fig.height == "default") 
      {subchunkify(barplot.sc.mc(x = results, xlab = "Häufigkeit"), fig_height = (1 + 0.75*nrow(results)), fig_width = 9)}
      else 
      {subchunkify(barplot.sc.mc(x = results, xlab = "Häufigkeit"), fig_height = fig.height, fig_width = 9)}
    }
    
    cat("  \n  \n")
    
  }
}

# merge-Funktion für Schulnoten
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

# merge-Funktion für den Workload (funktioniert, sollte überarbeitet werden)
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


# Funktion für Abbildungen analog zu alten EvaSys-Skalen 
merge.evasys.sk <- function(x, # Daten
                            inkl = "nr", # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                            nr = "", # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
                            show.alt = TRUE, # Zeige Ausweichoptionen, falls es sie gibt
                            number = 6, # Skala (OHNE AUSWEICHOPTIONEN!)
                            alt1 = FALSE, # Text für erste Ausweichoption (standardmäßig 0 in den Daten, siehe alt1.num)
                            alt2 = FALSE, # Text für zweite Ausweichoption (standardmäßig 7 in den Daten, siehe alt1.num)
                            alt1.num = 0, # Welche Zahl entspricht alt1
                            alt2.num = 7, # Welche Zahl entspricht alt2
                            lime = FALSE, # Für Daten im Format nach LimeSurvey Export (nach Syntax-Skript)
                            lime.brackets = FALSE, # Müssen eckige Klammern um den Fragetext herum entfernt werden?
                            show.plot = show.plot.sk, # Zeige Plot?
                            no.pagebreak = TRUE) # Seitenumbrüche verhindern?
{
  
  if (sum(!is.na(x)) > 0) {
    
    if (inkl == "nr") {
      if (nr == "") {inkl <- TRUE} else {inkl <- eval(parse(text = paste0("inkl.", nr)))}
    }
    
    if (inkl == TRUE) {
      
      if (lime == TRUE) {
        
        temp <- attr(x, "label")
        
        levs <- levels(x)
        x <- as.numeric(x, na.rm = TRUE) 
        
        for (l in 1:length(levs)) {
          x <- sjlabelled::add_labels(x, labels = setNames(l, levs[l]))
        }
        
        if(lime.brackets == TRUE) {
          temp <- sub("^\\[", "", temp)
          temp <- sub("].*$", "", temp)
        }
        
        attr(x, "label") <- temp
        
      }
      if(no.pagebreak == TRUE) {cat("\\begin{minipage}{\\linewidth} \n")}
      cat("\\subsubsection{ " , nr, " ", replace.latex.issues(attr(x, "label")), "}  \n  \n")
      cat("  \n  \n")
      
      xtab <- x
      xtab <- xtab[xtab %in% c(1:number)]
      print(table.stat.single(xtab, col1.name = "n", md = TRUE))
      
      cat("  \n  \n")
      
      if (show.alt == TRUE) {
        if(alt1 != FALSE) {
          
          cat("\\begin{center}Die Ausweichoption \"\\textit{", alt1, "}\" wurde ", sum(x == alt1.num, na.rm = TRUE), 
              " mal gewählt.\\end{center}  \n  \n", sep = "")
          
        }
        if(alt2 != FALSE) {cat("\\begin{center}Die Ausweichoption \"\\textit{", alt2, "}\" wurde ", sum(x == alt2.num, na.rm = TRUE), 
                               " mal gewählt.\\end{center}  \n  \n", sep = "")
        }
        
        labels <- names(attributes(x)$labels)
        tmin <- labels[1]
        tmax <- labels[number]
        
        cat("  \n \n")
        
        if(show.plot == TRUE) {
          subchunkify(evasys.skala.plot (x, tmin, tmax, number = number), fig_height = 1.4, fig_width = 6)
        }
        
      }
      if(no.pagebreak == TRUE) {
        cat("\n\\bigskip")
        cat("\n\\end{minipage}")
      }
      cat("   \n  \n")
    }
    
  }
}


# Funktion für die Abbildung numerischer (quasi metrisch, wie Alter, Abschlussnote, etc.)
# Kann auch für Balkendiagramme bei ordinalen Skalen genutzt werden
merge.num <- function(x, # Daten
                      inkl = "nr",  # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                      nr = "", # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
                      main = "", # Überschrift für Plot
                      xlab = "", # Beschriftung x-Achse
                      cut.breaks = "", # Soll es cuts geben? Wo sollen die "breaks" sein, siehe cut()
                      cut.labels = "", # Soll es cuts geben? Wo sollen die "labels" sein, siehe cut()
                      show.table = TRUE, # Soll die Tabelle gezeigt werden
                      fig.height = 6, # Höhe der Abbildung
                      cutoff = FALSE) # Soll es einen cutoff geben? Alle Werte >= cutoff werden zusammengefasst; Ist nicht mit cuts möglich!
{
  
  if (inkl == "nr") {
    if (nr == "") {inkl <- TRUE} else {inkl <- eval(parse(text = paste0("inkl.", nr)))}
  }
  
  if (inkl == TRUE) {
    
    if(cut.breaks[1] != "" & cutoff != FALSE) {
      stop("Es können nicht \"cut.breaks\" und \"cutoff\" != FALSE sein.")
    }
    
    if (sum(!is.na(x)) > 0) {
      
      
      
      cat("\\subsubsection{ " , nr, " ", replace.latex.issues(attr(x, "label")), "}  \n  \n")
      
      x <- as.numeric(gsub(",", ".", x)) # falls mit Komma
      
      
      if(show.table == TRUE){print(table.stat.single(as.numeric(x, na.rm = TRUE), 
                                                     col1.name = "n", md = TRUE))
        cat("  \n  \n")}
      
      if (cut.breaks[1] != "") {
        x <- cut(as.numeric(x, na.rm = TRUE), 
                 breaks = cut.breaks, 
                 labels = cut.labels)}
      if (cutoff != FALSE) {x[x >= cutoff] <- cutoff}
      
      subchunkify(barplot.freq(x, cutoff = cutoff, xlab = xlab, main = main), fig_width = 9, fig_height = fig.height)
      cat("  \n  \n")
      
      
      
    }
  }
}


# Funktion für offene Antworten
merge.open <- function(x, # Daten
                       inkl = "nr",  # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                       inkl.global = inkl.open, # Zweite inkl-Variable, die die globale Variable "inkl.open" abfragt. Kann auch in TRUE oder FALSE geändert werden
                       nr = "", # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
                       anchor = FALSE, # Falls über open.answers Anker kriiert wurden hier die Nummer angeben
                       freq = FALSE) # Sollen gleiche offene Antworten zusammengefasst werden? Dann werden auch Häufigkeiten angezeigt
{
  
  
  if (inkl == "nr") {
    if (nr == "") {inkl <- TRUE} else {inkl <- eval(parse(text = paste0("inkl.", nr)))}
  }
  
  if (inkl == TRUE && inkl.global == TRUE) {
    
    if (anchor != FALSE)
    {
      cat(paste0("\\hypertarget{", anchor, ".bottom}{}\\subsubsection{" , nr, " ", replace.latex.issues(attr(x, "label")), "}  \n  \n"))
      cat(paste0("\\hyperlink{", anchor, ".top}{zurück nach oben}  \n  \n"))
    } else {cat("\\subsubsection{ " , nr, " ", replace.latex.issues(attr(x, "label")), "}  \n  \n")}
    
    if(length(na.omit(x)) > 0) { # wenn mind. 1 offene Antwort
      
      x <- x[order(x)]
      x <- replace.latex.issues(x, all = FALSE)
      x <- as.data.frame(x[!is.na(x)])  
      
      
      if(freq == TRUE) {
        x <- data.frame(table(x))
        colnames(x) <- c("Antwort", "Häufigkeit")
#        print(lv.kable(x, col.width = c("388pt", "50pt"), striped = FALSE, escape = TRUE))
        print(lv.kable(x, col.width = c(137, 18), striped = FALSE, escape = TRUE))
        
        
      } else {
        
        colnames(x) <- "Antwort"
#        print(lv.kable(x, col.width = "450pt", striped = FALSE, escape = TRUE))
        print(lv.kable(x, col.width = 159, striped = FALSE, escape = TRUE))
      }
      
      
    } else {
      
      cat("\\textit{Keine offenen Antworten zu dieser Frage.}  \n")
      
    }
    
    cat("  \n  \n")
    
  }
}


# Funktion für LVE-Ausreißer: whiskers
# Ausreißer: Mehr als ein Interquartilsabstand vom Quartil entfernt
whiskers <- function(x, # Variable, geht auch für mehrere auf einmal; muss ein Dataframe sein!
                     kennung, # Kennung/Fallnummer zum Aggregieren
                     name, # Doz-Name; mit "paste(data$vorname, data$nachname)" kann Vor- und Nachname verwendet werden
                     lv_art, # LV-Art
                     no.pagebreak = TRUE) # Seitenumbrüche mittendrin verhindern?
{
  if(!is.data.frame(x)) {
    stop("x muss ein dataframe sein (damit Variablennamen vorhanden sind). Wenn Du einzelne Variablen nutzen möchtest, verwende z.B. data[\"Variablenname in Anführungsstriche\"] bei der Definition von x.")
  }
  
  for (k in 1:ncol(x)) {
    
    tmp <- aggr.data(x[, k], kennung = kennung)
    dfaggr <- data.frame(kennung, name, lv_art)
    dfaggr <- dfaggr[!duplicated(dfaggr$kennung), ]
    dfaggr <- cbind(dfaggr, tmp)[, c(1, 4, 2, 3)]
    colnames(dfaggr) <- c("Kennung", "M", "Dozent", "LV-Art")
    
    whisk_bot <- quantile(tmp, 0.25, na.rm=TRUE)- 
      (1.5 * (quantile(tmp, 0.75, na.rm=TRUE) - quantile(tmp, 0.25, na.rm=TRUE)))
    whisk_top <- quantile(tmp, 0.75, na.rm=TRUE)+ 
      (1.5 * (quantile(tmp, 0.75, na.rm=TRUE) - quantile(tmp, 0.25, na.rm=TRUE)))
    
    # Bottom
    if(no.pagebreak == TRUE) {cat("\\begin{minipage}{\\linewidth} \n")}
    cat(paste0("\\subsubsection{ ", colnames(x)[k], ": ", attr(x[, k], "label"), "}  \n  \n"))
    
    cat("\\center{Ausreißer nach unten/links:}  \n  \n")
    
    
    if(length(tmp[tmp < whisk_bot & !is.na(tmp)]) > 0) {
      
      tab <- dfaggr[dfaggr$M < whisk_bot & !is.na(dfaggr$M), ]
      tab$M <- round(tab$M, digits = 2)
      tab <- tab[order(tab$M), ]
      tab[tab == "Tutorium/Mentorium/Kolloqium"] <- "Tutorium/ Mentorium/ Kolloqium"
#      print(lv.kable(tab, col.width = c("50pt", "30pt", "100pt", "60pt")))
      print(lv.kable(tab, col.width = c(18, 11, 35, 21)))
      
      cat("  \n  \n")
      
    }  else {
      cat("*Keiner der Werte ist mehr als anderthalb Interquartilsabstände vom unteren Quartil entfernt.*  \n  \n")
      
    }
    
    # Top
    cat("\\bigskip \n")
    cat("\\center{Ausreißer nach oben/rechts:}  \n  \n")
    
    
    if(length(tmp[tmp > whisk_top & !is.na(tmp)]) > 0) {
      
      tab <- dfaggr[dfaggr$M > whisk_top & !is.na(dfaggr$M), ]
      tab$M <- round(tab$M, digits = 2)
      tab <- tab[order(tab$M), ]
      tab[tab == "Tutorium/Mentorium/Kolloqium"] <- "Tutorium/ Mentorium/ Kolloqium"
#      print(lv.kable(tab, col.width = c("50pt", "30pt", "100pt", "60pt")))
      print(lv.kable(tab, col.width = c(18, 11, 35, 21)))
      
      cat("  \n  \n")
      
    }  else {
      cat("*Keiner der Werte ist mehr als anderthalb Interquartilsabstände vom oberen Quartil entfernt.*  \n  \n")
      
    }
    
    cat("\\raggedright  \n  \n")
    cat("\\bigskip \n")
    cat("\\bigskip \n")
    if(no.pagebreak == TRUE) {
      cat("\n\\end{minipage} \n")
    }
  }
  
}