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
