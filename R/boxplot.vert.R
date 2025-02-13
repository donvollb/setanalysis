#' Vertikaler Boxplot
#'
#' @param x Daten
#' @param kennung Kennung
#' @param dont.know gibt es eine "weiß nicht"-Option?
#'
#' @returns Boxplot
#' @export

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
