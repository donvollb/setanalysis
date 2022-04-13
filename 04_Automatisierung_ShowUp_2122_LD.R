suppressPackageStartupMessages(if(!require(pacman)){install.packages("pacman")})
pacman::p_load(rmarkdown, markdown, knitr)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Datei anlegen, in die hinten immer reingeschrieben wird, auf wie vielen Personen der Bericht jeweils basiert:
personalized_info <- read.csv2("Tabellen/LD_ShowUp_2122.csv", check.names = FALSE, fileEncoding = "latin1")

protokoll <- personalized_info
protokoll$N <- NA
write.csv2(protokoll, file="Tabellen/LD_Berichte_Protokoll_ShowUp_2122.csv", row.names=FALSE)


#pdf-Dokumente erstellen

nBerichte <- nrow(personalized_info)


for (i in 1:nBerichte) { # 1:nBerichte

  knitr::knit_meta(class=NULL, clean = TRUE)
  rmarkdown::render(input = "03_Master_ShowUp_2122_LD.Rmd", # ggf. Dateipfad erg?nzen
                    output_format = "pdf_document",
                    output_dir = "./pdf_LD",
                    output_file = paste0(personalized_info$Dateiname[i])
                    # encoding="UTF-8"
                    )
}
######################################
### Berichte in Nextcloud kopieren ###
######################################

### Hier anfangen! ###


# Pfade für entsprechende Befragung eingeben
pfad.FB1 <- ""
pfad.FB2 <- ""
pfad.FB3 <- ""
pfad.FB4 <- ""
pfad.FB5 <- ""
pfad.FB6 <- ""
pfad.FB7 <- ""
pfad.FB8 <- ""
pfad.KO.2FB <- ""
pfad.LD.2FB <- ""
pfad.KO.ZFL <- ""
pfad.LD.ZFL <- ""


# Für welchen Campus sollen Berichte versendet werden? "KO" oder "LD" möglich!
campus <- "KO" 


# Protokoll erneut einlesen
if(campus == "KO") {
  protokoll <- read.csv2("Tabellen/KO_Berichte_Protokoll_2FB_ABS_2021.csv", check.names = FALSE)
} else {
  protokoll <- read.csv2("Tabellen/LD_Berichte_Protokoll_2FB_ABS_2021.csv", check.names = FALSE)
}


# Befehl zum Kopieren der Berichte 
if (winDialog(type = "yesno", 
              paste0("Möchtest Du die Berichte wirklich veröffentlichen? Es ist der Campus ",
                     campus, " ausgewählt.")) == "YES") {
  
  if (campus == "KO") {
    pfad.2FB <- pfad.KO.2FB
    pfad.ZFL <- pfad.KO.ZFL
  }
  
  if (campus == "LD") {
    pfad.2FB <- pfad.LD.2FB
    pfad.ZFL <- pfad.LD.ZFL
  }
  
  for (k in 1:nrow(protokoll)) {
    
    if (protokoll$N[k] > 9) {
    # if (is.na(protokoll$N[k])) { ## Zum Testen, falls 
      pfad.pdf <- paste0("./Berichte-final_2FB-ABS_2021", "/", protokoll$Dateiname[k], ".pdf")
      
      ordner <- names(protokoll[which(protokoll[k, ] == "x")])
      
      if (length(ordner > 0)) {
        ordner <- paste0("pfad.", ordner)
        
        for (l in ordner) {
          file.copy(from = pfad.pdf,
                    to = eval(parse(text = l)),
                    overwrite=TRUE)
          
        }
      }
      rm(pfad.pdf, ordner)
    }
  }
}
