#' Barplot-Boxplot Hypbrid für die Darstellung von ordinalskalierten Variablen
#'
#' @description
#' Die optimale Chunk-Einstellung hierfür ist: fig.width = 6, fig.height = 1.4
#' 
#' @param x Daten
#' @param tmin Beschriftung links
#' @param tmax Beschriftung rechts
#' @param number Skala (6 für Sechserskala etc.)
#'
#' @examples
#' barplot_evasys(BspDaten$dataSHOWUP$info_ausr_studgang,
#'                tmin = "stimme gar nicht zu", tmax = "stimme voll zu")
#' 
#'
#' @returns Barplot-Boxplot-Hybrid
#' @export

barplot_evasys <- function(x, # Daten
                           tmin, # Beschriftung links
                           tmax, # Beschriftung rechts
                           number = 6) # Skala (6 für Sechserskala etc.)
{
  x[!(x %in% c(1:number))] <- NA
  x <- x[!is.na(x)]
  
  tmin <- sapply(tmin, \(x) paste(strwrap(x, width = 15), collapse = "\n"))
  tmax <- sapply(tmax, \(x) paste(strwrap(x, width = 15), collapse = "\n"))

  line.tmin <- ifelse(grepl("\\n", tmin), -1.7, -2)
  line.tmax <- ifelse(grepl("\\n", tmax), -1.7, -2)
  
  bobby <- x |>
    psych::describe() |>
    round(2) |>
    data.frame() |>
    subset(select = c("n", "mean", "sd", "min", "max")) |>
    data.frame()
  
  xtab <- table(c(x, 1:number)) #- 1 # damit alle angezeigt werden
  
  .common_par(mar = c(2, 5.3, 2.1, 5.3))
  
  .empty_plot(xlim = c(0.2, number * 1.2), ylim = c(0, sum(table(x))))
  
  #barplot(rep(NA, number),ylim=c(0,sum(table(x))),axes=FALSE)
  abline(v = seq(0.7, -0.5 + 1.2 * number, by = 1.2), col = "grey80")
  
  .costum_barplot(xtab)
  # .costum_boxplot(- 0.5 + 1.2 * x, at = length(x) * 0.75, boxwex = 0, median = FALSE, mean = TRUE,
  #                 staplewex = 0.6, staplelwd = 2,
  #                 boxlwd = 3, whisklty = 1, whisklwd = 2)
  
  .text_bottom(1:number, at = seq(0.7, -0.5 + 1.2 * number, by = 1.2))  
  .text_top(paste(sprintf("%.1f", 100 * prop.table(xtab)), "%"),
            at = seq(0.7, -0.5 + 1.2 * number, by = 1.2))

  
  par(new = TRUE)
  
  boxplot(c(mean(x) - sd(x), rep(mean(x), 3), mean(x) + sd(x)),
          medcol = setanalysis_defaults$color.bars,
          horizontal = TRUE, range = 0, ylim = c(0.6, number + 0.4), medlwd = 4,
          boxlwd = 0, xlim = c(0.3,1.3), whisklty = 1)
          
          # boxcol = "#8C0000", staplewex = 0.6, staplelwd = 2,
          # boxlwd = 3, boxwex = 0, outline = FALSE,
          # whisklty = 1, whisklwd = 2, axes = FALSE)
  
  bxp <- boxplot(as.numeric(x), plot=FALSE)
  bxp$stats <- matrix(c((bobby$mean-bobby$sd), bobby$mean, bobby$mean, bobby$mean, (bobby$mean+bobby$sd)))
  invisible(ifelse(bxp$stats[5,1] > number, bxp$stats[5,1] <- number, bxp$stats[5,1] <- bxp$stats[5,1]))
  invisible(ifelse(bxp$stats[1,1] < 1, bxp$stats[1,1] <- 1, bxp$stats[1,1] <- bxp$stats[1,1]))
  .text_left(tmin)
  .text_right(tmax)

  cat("  \n  \n")
}
