beispiel <- function(datei) system.file("extdata", datei, package = "setanalysis")

test_that("input.tabelle() erzeugt die inkl.- und header-Spalten unverändert", {
  berichte <- input.tabelle(beispiel("beispiel_berichte.xlsx"),
                            beispiel("beispiel_regeln.xlsx"))

  expect_s3_class(berichte, "data.frame")
  expect_snapshot(print(as.data.frame(berichte)))
})

test_that("input.tabelle() schaltet im Master-Bericht (erste Zeile) alles ein", {
  berichte <- input.tabelle(beispiel("beispiel_berichte.xlsx"),
                            beispiel("beispiel_regeln.xlsx"))
  inkl_spalten <- grep("^(inkl|header)", names(berichte), value = TRUE)

  expect_true(all(unlist(berichte[1, inkl_spalten])))
})
