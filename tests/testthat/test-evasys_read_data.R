# Fiktiver evasys-Export aus inst/extdata, erzeugt mit data-raw/beispiel_evasys.R
fixture <- function(datei) system.file("extdata", paste0("beispiel_", datei), package = "setanalysis")

test_that("evasys_read_data() bereitet Rohdaten und Codebuch unverändert auf", {
  daten <- evasys_read_data(fixture("evasys_rohdaten.csv"), fixture("evasys_codebuch.csv"))

  expect_snapshot_value(daten, style = "deparse")
})

test_that("evasys_read_data() setzt Label, Nummer und Fragetyp", {
  daten <- evasys_read_data(fixture("evasys_rohdaten.csv"), fixture("evasys_codebuch.csv"))

  # "[FILTER]"-Präfix im Spaltennamen wird entfernt, MC-Optionen sind nummeriert
  expect_named(daten, c("semester", "zufrieden", "abschluss_1", "abschluss_2", "kommentar", "alter"))
  expect_identical(
    vapply(daten, attr, character(1), which = "type"),
    c(
      semester = "sc", zufrieden = "sk", abschluss_1 = "mc", abschluss_2 = "mc",
      kommentar = "open/num", alter = "open/num"
    )
  )
  expect_identical(attr(daten$zufrieden, "nr"), "1.2")
  expect_identical(
    attr(daten$semester, "labels"),
    c("erstes Semester" = 1, "höheres Semester" = 2)
  )
  # Platzhalter wie "-", ".", "/" und "[Freitextfeld]" werden zu NA
  expect_identical(sum(is.na(daten$kommentar)), 3L)
  expect_identical(sum(is.na(daten$alter)), 2L)
})

test_that("Ergebnis von evasys_read_data() lässt sich direkt auswerten", {
  daten <- evasys_read_data(fixture("evasys_rohdaten.csv"), fixture("evasys_codebuch.csv"))

  expect_report_snapshot(
    merge_many(daten, nr_auto = FALSE, multi.sk = FALSE),
    "evasys_read_data-merge_many"
  )
})
