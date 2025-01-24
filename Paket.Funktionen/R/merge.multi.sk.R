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
