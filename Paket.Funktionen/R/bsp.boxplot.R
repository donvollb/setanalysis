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
