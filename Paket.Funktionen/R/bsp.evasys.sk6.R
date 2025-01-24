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
