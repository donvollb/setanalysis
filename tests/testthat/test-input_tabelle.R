beispiel <- function(datei) system.file("extdata", datei, package = "setanalysis")

test_that("input_tabelle() erzeugt die inkl.- und header-Spalten unverändert", {
  berichte <- input_tabelle(
    beispiel("beispiel_berichte.xlsx"),
    beispiel("beispiel_regeln.xlsx")
  )

  expect_s3_class(berichte, "data.frame")
  expect_snapshot(print(as.data.frame(berichte)))
})

test_that("input_tabelle() schaltet im Master-Bericht (erste Zeile) alles ein", {
  berichte <- input_tabelle(
    beispiel("beispiel_berichte.xlsx"),
    beispiel("beispiel_regeln.xlsx")
  )
  inkl_spalten <- grep("^(inkl|header)", names(berichte), value = TRUE)

  expect_true(all(unlist(berichte[1, inkl_spalten])))
})

test_that("Regeln werden direkt mit den Spalten der Berichtstabelle ausgewertet", {
  berichte <- data.frame(
    Art = c("speziell", "alles", "Studiengang"),
    Fach = c("Art und Weise", "Mathematik", "Physik")
  )
  regel <- function(bedingung, var = "inkl.1.1", alle_vars = var) {
    .evaluate_rule(var, bedingung, rule_vars = alle_vars, data = berichte, env = globalenv())
  }

  expect_identical(regel('Art != "speziell"'), c(FALSE, TRUE, TRUE))
  expect_identical(regel("immer TRUE"), c(TRUE, TRUE, TRUE))
  expect_identical(regel("immer FALSE"), c(FALSE, FALSE, FALSE))

  # Ohne Leerzeichen nach dem Spaltennamen (früher: Abbruch)
  expect_identical(regel('Art=="alles"'), c(FALSE, TRUE, FALSE))
  # Spaltenname innerhalb eines Textes (früher: Text wurde verfälscht)
  expect_identical(regel('Fach == "Art und Weise"'), c(TRUE, FALSE, FALSE))
})

test_that("header-Regeln fassen die inkl.-Spalten ihres Abschnitts zusammen", {
  berichte <- data.frame(
    inkl.2.1 = c(TRUE, FALSE, FALSE, NA),
    inkl.2.2 = c(FALSE, FALSE, TRUE, FALSE)
  )
  vars <- c("inkl.2.1", "inkl.2.2", "header2")

  expect_identical(
    .evaluate_rule("header2", "eine der inkl.2.x-Variablen == TRUE", vars, berichte, globalenv()),
    c(TRUE, FALSE, TRUE, NA)
  )
})
