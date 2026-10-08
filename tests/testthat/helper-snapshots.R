# Hilfsfunktionen für die Charakterisierungstests --------------------------
#
# Die Tests halten die aktuelle Ausgabe des Pakets als Snapshots fest
# (Markdown/Typst-Text und SVG-Abbildungen in tests/testthat/_snaps/).
# Ändert sich die Ausgabe, schlägt der Test fehl; gewollte Änderungen
# werden mit testthat::snapshot_review() geprüft und übernommen.

# Ausgangszustand der Paketeinstellungen (direkt nach dem Laden)
defaults_original <- as.list(setanalysis_defaults)

# Globalen Zustand des Pakets zurücksetzen --------------------------------

reset_setanalysis_state <- function() {
  list2env(defaults_original, envir = setanalysis_defaults)
  rm(list = ls(list_open_answers, all.names = TRUE), envir = list_open_answers)
  list_open_answers$anchor.nr <- 0
  .subchunk_env$counter <- 0
}

# Globale inkl.-Variablen für die Dauer eines Tests setzen ----------------
# Beispiel: local_inkl(`2.1` = TRUE, `2.2` = FALSE) legt inkl.2.1 und
# inkl.2.2 in der globalen Umgebung an (so wie im Berichts-Template).

local_inkl <- function(..., .env = parent.frame()) {
  werte <- list(...)
  namen <- paste0("inkl.", names(werte))
  for (k in seq_along(werte)) assign(namen[k], werte[[k]], envir = globalenv())
  withr::defer(rm(list = namen, envir = globalenv()), envir = .env)
}

# Snapshot der Berichtsausgabe (cat-Ausgabe + Abbildungen) ----------------

expect_report_snapshot <- function(code, name) {
  reset_setanalysis_state()
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  warnungen <- character()
  ausgabe <- withCallingHandlers(
    utils::capture.output(code),
    warning = function(w) {
      warnungen <<- c(warnungen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  if (length(warnungen) > 0) {
    ausgabe <- c(ausgabe, "", "<!-- Warnungen:", paste("-", warnungen), "-->")
  }

  md_pfad <- file.path(dir, paste0(name, ".md"))
  writeLines(ausgabe, md_pfad)
  expect_snapshot_file(md_pfad, paste0(name, ".md"), compare = compare_file_text)

  abbildungen <- sort(list.files(file.path(dir, "figure"), full.names = TRUE))
  for (abb in abbildungen) {
    expect_snapshot_file(abb, paste0(name, "-", basename(abb)),
      compare = compare_file_text
    )
  }
}

# Snapshot einer einzelnen Abbildung --------------------------------------

expect_plot_snapshot <- function(code, name, width = 9, height = 2) {
  reset_setanalysis_state()
  pfad <- file.path(withr::local_tempdir(), paste0(name, ".svg"))
  svglite::svglite(pfad, width = width, height = height)
  tryCatch(force(code), finally = grDevices::dev.off())
  expect_snapshot_file(pfad, paste0(name, ".svg"), compare = compare_file_text)
}

# Snapshot einer tinytable-Tabelle (als Typst-Code) -----------------------

expect_table_snapshot <- function(code, name) {
  reset_setanalysis_state()
  tabelle <- force(code)
  expect_s4_class(tabelle, "tinytable")
  pfad <- file.path(withr::local_tempdir(), paste0(name, ".typ"))
  writeLines(tinytable::save_tt(tabelle, output = "typst"), pfad)
  expect_snapshot_file(pfad, paste0(name, ".typ"), compare = compare_file_text)
}
