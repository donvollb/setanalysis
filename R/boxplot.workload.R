#' Boxplot mit Workloads der LVs: Funktioniert, sollte überarbeitet werden
#' 
#' @param x Daten
#' @param p Text/Beschriftungen Y Achse
#' @param q Skala x Achse
#' @param d Anzahl von p
#' @param e Anzahl von q
#' @param nums ?
#'
#' @returns Boxplot
#' @export

boxplot.workload <- function(x,p,q,d,e, nums) # x = daten, p = Text/Beschriftungen Y Achse, q = Skala x Achse, d = Anzahl von p, e = Anzahl von q
{
  opar <- par(no.readonly = TRUE)
  par(fg="gray80")
  par(family = settings$font.family)
  #par(mar=c(7, 4.1, 4.1, 4.2))
  par(mar=c(7, 4.1, 2, 4.2))
  boxplot(x, col= settings$color.bars, border="black", ylab=NULL, xlab=NULL, horizontal = TRUE, ylim=c(0,e),
          xaxt="n", yaxt="n", boxwex = 0.8, pars=list(outcol = settings$color.bars, outpch=20))
  abline(v = c(0:e))
  boxplot(x, col= settings$color.bars, border="black", ylab=NULL, xlab=NULL, horizontal = TRUE, ylim=c(0,e),
          xaxt="n", yaxt="n", boxwex = 0.8, pars=list(outcol = settings$color.bars, outpch=20), add = TRUE)
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