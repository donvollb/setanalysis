test_that("barplot_freq() zeichnet unverändert", {
  expect_plot_snapshot(
    barplot_freq(BspDaten$Plots$num,
                 xlab = "Durchschnittsnote für Hochschulzugangsberechtigung"),
    "barplot_freq-num", height = 6
  )
  expect_plot_snapshot(barplot_freq(BspDaten$Plots$fsem, xlab = "Fachsemester"),
                       "barplot_freq-fachsemester", width = 10, height = 5)
})

test_that("barplot_scmc() zeichnet unverändert", {
  expect_plot_snapshot(barplot_scmc(BspDaten$Plots$sc, xlab = "Häufigkeit"),
                       "barplot_scmc-sc", height = 3)
  expect_plot_snapshot(barplot_scmc(BspDaten$Plots$mc, xlab = "Häufigkeit"),
                       "barplot_scmc-mc", height = 7)
})

test_that("barplot_scmc() meldet fehlende Daten", {
  leer <- data.frame(label = c("a", "b"), freq = c(0, 0), perc = c(0, 0))
  expect_snapshot(barplot_scmc(leer))
})

test_that("barplot_sk() zeichnet unverändert", {
  expect_plot_snapshot(
    barplot_sk(BspDaten$dataSHOWUP$info_ausr_studgang,
               tmin = "stimme gar nicht zu", tmax = "stimme voll zu"),
    "barplot_sk-sechserskala"
  )
  expect_plot_snapshot(
    barplot_sk(BspDaten$dataLVE$KF_01, number = 5,
               tmin = "trifft gar nicht zu", tmax = "trifft voll zu"),
    "barplot_sk-fuenferskala"
  )
})

test_that("boxplot_aggr_sk() zeichnet unverändert", {
  expect_plot_snapshot(
    boxplot_aggr_sk(BspDaten$Plots$aggr.data, BspDaten$Plots$aggr.labels,
                    BspDaten$Plots$aggr.skala),
    "boxplot_aggr_sk-sechserskala", height = 16
  )
  expect_plot_snapshot(
    boxplot_aggr_sk(BspDaten$Plots$aggr.data[, 1:3] - 1,
                    BspDaten$Plots$aggr.labels[1:3],
                    c("links", "", "Mitte", "", "rechts")),
    "boxplot_aggr_sk-fuenferskala", height = 4.5
  )
})

test_that("boxplot_grade(), boxplot_rueck() und boxplot_wl() zeichnen unverändert", {
  expect_plot_snapshot(boxplot_grade(BspDaten$Plots$grade), "boxplot_grade")
  expect_plot_snapshot(boxplot_rueck(BspDaten$Plots$rueck), "boxplot_rueck")
  expect_plot_snapshot(boxplot_wl(BspDaten$Plots$WL), "boxplot_wl", height = 4)
})

test_that("Legenden-Abbildungen (bsp.*) bleiben unverändert", {
  expect_report_snapshot(bsp.boxplot(), "bsp.boxplot")
  expect_report_snapshot(bsp.evasys.sk6(), "bsp.evasys.sk6")
})
