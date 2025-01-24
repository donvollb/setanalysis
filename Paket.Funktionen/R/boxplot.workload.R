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
