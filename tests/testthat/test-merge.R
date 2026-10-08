# Testdaten ---------------------------------------------------------------

lve <- BspDaten$dataLVE
showup <- BspDaten$dataSHOWUP
abschluesse <- showup[, paste0("abschluss_", 1:8)]
kf_123 <- lve[, c("KF_01", "KF_02", "KF_03")]

# merge_sc() --------------------------------------------------------------

test_that("merge_sc() gibt SC-Fragen unverändert aus", {
  expect_report_snapshot(merge_sc(lve$V3_D), "merge_sc-standard")
  expect_report_snapshot(
    merge_sc(lve$FachSemN,
      order.table = "decreasing", col2.name = "Anzahl",
      digits = 2, fig.height = 8
    ),
    "merge_sc-optionen"
  )
  expect_report_snapshot(
    merge_sc(lve$V3_D, show.plot = FALSE, pagebreak = TRUE),
    "merge_sc-ohne-plot-mit-umbruch"
  )
  expect_report_snapshot(
    merge_sc(BspDaten$Tabellen$freq, already.labels = TRUE),
    "merge_sc-bereits-labels"
  )
})

test_that("merge_sc() und merge_mc() brechen lange Antwortoptionen unverändert um", {
  lang <- c(
    "Eine sehr lange Antwortoption, die in der Abbildung umbrochen werden muss",
    "kurz",
    # erste Zeile genau 39 Zeichen -> prüft die exakte Umbruchbreite
    "Antwortoption mit genau neununddreißig! Zeichen in der ersten Zeile"
  )
  sc <- factor(lang[c(1, 1, 2, 3, 3, 3, NA)], levels = lang)
  attr(sc, "label") <- "Frage mit langen Antwortoptionen"
  expect_report_snapshot(merge_sc(sc, already.labels = TRUE), "merge_sc-lange-labels")

  mc <- data.frame(a = c(1, 0, 1, 1), b = c(0, 2, 2, 0), c = c(3, 3, 0, 0))
  for (k in seq_along(lang)) {
    attr(mc[[k]], "label") <- paste("Frage mit langen Antwortoptionen:", lang[k])
  }
  expect_report_snapshot(merge_mc(mc), "merge_mc-lange-labels")
})

test_that("merge_sc() gibt ohne gültige Werte nichts aus", {
  x <- lve$V3_D
  x[] <- NA
  expect_length(capture.output(merge_sc(x)), 0)
})

# merge_mc() --------------------------------------------------------------

test_that("merge_mc() gibt MC-Fragen unverändert aus", {
  expect_report_snapshot(merge_mc(abschluesse), "merge_mc-standard")
  expect_report_snapshot(
    merge_mc(abschluesse,
      head = "Abschluss", col1.name = "Abschluss",
      order.table = "decreasing", digits = 0, fig.height = 6
    ),
    "merge_mc-optionen"
  )
  expect_report_snapshot(
    merge_mc(abschluesse, valid.perc = FALSE, show.plot = FALSE),
    "merge_mc-ohne-gueltige-prozent"
  )
})

test_that("merge_mc() verarbeitet LimeSurvey-Daten unverändert", {
  optionen <- c("Bachelor", "Master", "Lehramt")
  lime <- data.frame(a = c(1, 2, 1, NA, 2), b = c(2, 1, 1, NA, 2), c = c(2, 2, 1, NA, 1))
  for (k in seq_along(optionen)) {
    attr(lime[, k], "label") <- paste0("[", optionen[k], "] Welchen Abschluss streben Sie an?")
  }
  expect_report_snapshot(
    merge_mc(lime, lime = TRUE, filter = "FILTER_1"),
    "merge_mc-limesurvey"
  )
})

# merge_sk() --------------------------------------------------------------

test_that("merge_sk() gibt Skalenfragen unverändert aus", {
  expect_report_snapshot(merge_sk(showup$info_ausr_studgang), "merge_sk-standard")
  expect_report_snapshot(
    merge_sk(lve$KF_01, alt1 = "kann ich nicht beurteilen", alt2 = "trifft nicht zu"),
    "merge_sk-ausweichoptionen"
  )
  expect_report_snapshot(merge_sk(lve$KF_01, show.plot = FALSE), "merge_sk-ohne-plot")
  expect_report_snapshot(merge_sk(lve$KF_01, show.alt = FALSE), "merge_sk-ohne-alt")
})

test_that("merge_sk() verarbeitet LimeSurvey-Daten unverändert", {
  x <- factor(lve$KF_01[1:60],
    levels = 1:6,
    labels = c("trifft gar nicht zu", "2", "3", "4", "5", "trifft voll zu")
  )
  attr(x, "label") <- "[Die Veranstaltung war gut strukturiert.] Zusatztext"
  expect_report_snapshot(
    merge_sk(x, lime = TRUE, lime.brackets = TRUE),
    "merge_sk-limesurvey"
  )
})

test_that("merge_sk() gibt ohne gültige Werte nichts aus", {
  x <- lve$KF_01
  x[] <- NA
  expect_length(capture.output(merge_sk(x)), 0)
})

# merge_aggr_sk() ---------------------------------------------------------

test_that("merge_aggr_sk() gibt mehrere Skalenfragen unverändert aus", {
  expect_report_snapshot(
    merge_aggr_sk(kf_123, number = 6, aggr = TRUE, kennung = lve$Kennung),
    "merge_aggr_sk-aggregiert"
  )
  expect_report_snapshot(
    merge_aggr_sk(kf_123,
      alt1 = "weiß nicht", alt1.num = 0, col2.name = "N",
      message = "Hinweistext  \n\n", fig.height = 5
    ),
    "merge_aggr_sk-optionen"
  )
  expect_report_snapshot(
    merge_aggr_sk(kf_123, tmin = "links", tmax = "rechts", show.table = FALSE),
    "merge_aggr_sk-eigene-pole"
  )
  expect_report_snapshot(
    merge_aggr_sk(kf_123, show.plot = FALSE),
    "merge_aggr_sk-ohne-plot"
  )
})

test_that("merge_aggr_sk() zählt Ausweichoptionen unverändert", {
  items <- kf_123[1:40, 1:2]
  for (n in names(items)) attributes(items[[n]]) <- attributes(kf_123[[n]])
  items$KF_01[c(1, 5, 9)] <- 0 # erste Ausweichoption
  items$KF_01[c(2, 3)] <- 7 # zweite Ausweichoption
  items$KF_02[c(4, 6, 8, 10)] <- 7

  expect_report_snapshot(
    merge_aggr_sk(items,
      alt1 = "weiß nicht", alt2 = "trifft nicht zu",
      alt1.num = 0, alt2.num = 7, show.plot = FALSE
    ),
    "merge_aggr_sk-ausweichoptionen-gezaehlt"
  )
})

test_that("merge_aggr_sk() bricht bei unterschiedlicher Stufenanzahl ab", {
  gemischt <- data.frame(a = lve$KF_01, b = lve$V3_D)
  expect_error(merge_aggr_sk(gemischt), "unterschiedliche")
})

# merge_num() und merge_fachsem() -----------------------------------------

test_that("merge_num() gibt numerische Fragen unverändert aus", {
  expect_report_snapshot(
    merge_num(showup$zugang_note,
      xlab = "Durchschnittsnote für Hochschulzugangsberechtigung",
      cut.breaks = c(0, 1.4, 1.9, 2.4, 2.9, 3.4, 2000),
      cut.labels = c(
        "1,0 bis 1,4", "1,5 bis 1,9", "2,0 bis 2,4",
        "2,5 bis 2,9", "3,0 bis 3,4", "3,5 bis 4,0"
      )
    ),
    "merge_num-cuts"
  )
  expect_report_snapshot(
    merge_num(lve$FachSemN,
      xlab = "Fachsemester", cutoff = 12, show.table = FALSE,
      fig.height = 4
    ),
    "merge_num-cutoff"
  )
})

test_that("merge_num() prüft Argumente und leere Daten", {
  expect_error(
    merge_num(lve$FachSemN, cut.breaks = c(0, 5, 99), cutoff = 12),
    "cut.breaks"
  )
  x <- showup$zugang_note
  x[] <- NA
  expect_length(capture.output(merge_num(x)), 0)
})

test_that("merge_fachsem() gibt Fachsemester unverändert aus", {
  expect_report_snapshot(merge_fachsem(lve$FachSemN), "merge_fachsem-alle")
  expect_report_snapshot(
    merge_fachsem(lve$FachSemN, group = "b", cutoff = 8),
    "merge_fachsem-bachelor"
  )
  expect_report_snapshot(
    merge_fachsem(lve$FachSemN, group = "m"),
    "merge_fachsem-master"
  )
})

test_that("merge_fachsem() meldet eine ungültige Gruppe verständlich", {
  expect_error(merge_fachsem(lve$FachSemN, group = "x"), "`group` muss")
})

# merge_grade(), merge_rueck(), merge_wl() --------------------------------

test_that("merge_grade() gibt Gesamtnoten unverändert aus", {
  expect_report_snapshot(
    merge_grade(lve$Note, kennung = lve$Kennung),
    "merge_grade-standard"
  )
  expect_report_snapshot(
    merge_grade(BspDaten$Plots$grade, already.aggr = TRUE, show.table = FALSE),
    "merge_grade-bereits-aggregiert"
  )
})

test_that("merge_rueck() gibt den Rücklauf unverändert aus", {
  expect_report_snapshot(merge_rueck(lve$Teilnehmer, lve$Kennung), "merge_rueck")
})

test_that("merge_wl() gibt den Workload unverändert aus", {
  expect_report_snapshot(merge_wl(lve$WL, lve$Kennung), "merge_wl-standard")
  expect_report_snapshot(
    merge_wl(BspDaten$Plots$WL, already.aggr = TRUE),
    "merge_wl-bereits-aggregiert"
  )
})

# merge_subj() ------------------------------------------------------------

test_that("merge_subj() fasst 1. und 2. Fach unverändert zusammen", {
  expect_report_snapshot(
    merge_subj(showup$fach1_2FB, showup$fach2_2FB),
    "merge_subj"
  )
})

# merge_open() und appendix_open() ----------------------------------------

test_that("merge_open() gibt offene Antworten ohne Anhang unverändert aus", {
  expect_report_snapshot(
    merge_open(showup$offen, appendix = FALSE),
    "merge_open-ohne-anhang"
  )

  doppelt <- c("Mehr Infos", "mehr infos", "Mehr Infos", "Klips-Hilfe", NA, " Videos ")
  attr(doppelt, "label") <- "Was hat gefehlt?"
  expect_report_snapshot(merge_open(doppelt, appendix = FALSE), "merge_open-haeufigkeiten")
  expect_report_snapshot(
    merge_open(doppelt, appendix = FALSE, freq = FALSE),
    "merge_open-ohne-haeufigkeiten"
  )

  leer <- c(NA_character_, NA_character_)
  attr(leer, "label") <- "Leere Frage"
  expect_report_snapshot(merge_open(leer, appendix = FALSE), "merge_open-leer")
})

test_that("merge_open() mit Anhang verweist und appendix_open() sammelt", {
  leer <- c(NA_character_, NA_character_)
  attr(leer, "label") <- "Leere Frage"
  local_inkl(`1.34` = TRUE, `1.35` = TRUE)

  expect_report_snapshot(
    {
      merge_open(showup$offen, nr = "1.34")
      merge_open(leer, nr = "1.35")
      appendix_open()
    },
    "merge_open-mit-anhang"
  )
})

test_that("appendix_open() übernimmt keine Antworten aus einem vorherigen Bericht", {
  leer <- c(NA_character_, NA_character_)
  attr(leer, "label") <- "Leere Frage"
  local_inkl(`1.34` = TRUE, `1.35` = TRUE)
  reset_setanalysis_state()
  withr::local_dir(withr::local_tempdir())

  # Bericht 1 mit Anhang
  invisible(capture.output({
    merge_open(showup$offen, nr = "1.34")
    appendix_open()
  }))
  expect_identical(list_open_answers$anchor.nr, 0)
  expect_identical(ls(list_open_answers), "anchor.nr")

  # Bericht 2 in derselben R-Sitzung: Anhang enthält nur die eigene Frage
  bericht2 <- capture.output({
    merge_open(leer, nr = "1.35")
    appendix_open()
  })
  expect_true(any(grepl("Leere Frage", bericht2)))
  expect_false(any(grepl("Welche weiteren Informationen", bericht2)))
})

test_that("appendix_open() übernimmt inkl = TRUE aus merge_open()", {
  withr::local_dir(withr::local_tempdir())

  bericht <- function(inkl) {
    reset_setanalysis_state()
    capture.output({
      merge_open(showup$offen, nr = "9.9", inkl = inkl, appendix = TRUE)
      appendix_open()
    })
  }

  # Referenz: Frage über die inkl.-Variable eingeschlossen
  ueber_inkl_variable <- local({
    local_inkl(`9.9` = TRUE)
    bericht(inkl = "nr")
  })

  # Ohne inkl.-Variable, Frage direkt mit inkl = TRUE eingeschlossen
  expect_false(exists("inkl.9.9"))
  expect_identical(bericht(inkl = TRUE), ueber_inkl_variable)

  # inkl = TRUE gilt auch dann, wenn die inkl.-Variable FALSE ist
  local({
    local_inkl(`9.9` = FALSE)
    expect_identical(bericht(inkl = TRUE), ueber_inkl_variable)
  })
})

test_that("appendix_open() gibt ohne vorherige offene Fragen nichts aus", {
  reset_setanalysis_state()
  expect_length(capture.output(appendix_open()), 0)
})

test_that("merge_open() respektiert inkl_global", {
  expect_length(capture.output(merge_open(showup$offen, inkl_global = FALSE)), 0)
})

# merge_auto() und merge_many() -------------------------------------------

test_that("merge_auto() erkennt die Fragetypen unverändert", {
  expect_report_snapshot(
    {
      merge_auto(lve$V3_D, nr_auto = FALSE)
      merge_auto(lve$KF_01, nr_auto = FALSE)
      merge_auto(showup$offen, nr_auto = FALSE)
      merge_auto(showup$zugang_note, nr_auto = FALSE)
      merge_auto(abschluesse, nr_auto = FALSE)
      merge_auto(kf_123, nr_auto = FALSE)
    },
    "merge_auto"
  )
})

test_that("merge_auto() unterscheidet offene und numerische Textantworten", {
  zahlen_als_text <- c("1,7", "2,3", NA, "3,0")
  attr(zahlen_als_text, "label") <- "Note als Text"
  attr(zahlen_als_text, "type") <- "open/num"
  expect_report_snapshot(
    merge_auto(zahlen_als_text, nr_auto = FALSE),
    "merge_auto-zahlen-als-text"
  )
})

test_that("merge_many() wertet mehrere Fragen unverändert aus", {
  expect_report_snapshot(
    merge_many(showup[, 1:12], nr_auto = FALSE),
    "merge_many-showup"
  )
  expect_report_snapshot(
    merge_many(lve[, c("KF_01", "KF_02", "KF_03", "V3_D")], nr_auto = FALSE),
    "merge_many-mehrere-skalen"
  )
  expect_report_snapshot(
    merge_many(lve[, c("KF_01", "V3_D")], multi.sk = FALSE, nr_auto = FALSE),
    "merge_many-einzelne-skalen"
  )
  expect_report_snapshot(
    merge_many(lve$V3_D, nr_auto = FALSE),
    "merge_many-einzelne-spalte"
  )
})

test_that("merge_many() meldet nicht unterstützte Typen", {
  x <- data.frame(a = 1:3)
  attr(x$a, "type") <- "xyz"
  expect_error(merge_many(x), "nicht unterstützten Typ")
})

# Korrigierte Fehler in merge_many() ---------------------------------------

test_that("merge_many() wertet auch eine Skalenfrage als letzte Spalte aus", {
  # früher: Abbruch mit „undefined columns selected“
  expect_report_snapshot(merge_many(showup, nr_auto = FALSE), "merge_many-letzte-spalte-sk")
})

test_that("merge_many() wertet eine einzelne Skalenfrage einzeln aus", {
  # früher: Abbruch mit „object 'counter' not found“
  ausgabe <- function(code) {
    reset_setanalysis_state()
    withr::local_dir(withr::local_tempdir())
    capture.output(code)
  }
  expect_identical(
    ausgabe(merge_many(lve[, c("KF_01", "V3_D")], nr_auto = FALSE)),
    ausgabe(merge_many(lve[, c("KF_01", "V3_D")], nr_auto = FALSE, multi.sk = FALSE))
  )
})

test_that("merge_many() wird nicht von einer globalen Variable `counter` gestört", {
  # früher: Abbruch mit „only 0's may be mixed with negative subscripts“
  ausgabe <- function() {
    reset_setanalysis_state()
    withr::local_dir(withr::local_tempdir())
    capture.output(merge_many(showup[, 1:12], nr_auto = FALSE))
  }
  ohne_counter <- ausgabe()

  assign("counter", 5, envir = globalenv())
  withr::defer(rm("counter", envir = globalenv()))
  expect_identical(ausgabe(), ohne_counter)
})

test_that("merge_many() meldet Spalten ohne Typ verständlich", {
  expect_error(merge_many(data.frame(a = 1:3, b = 1:3)), "hat keinen Typ")
})

# Weitere korrigierte Fehler ---------------------------------------------

test_that("merge_fachsem() berücksichtigt fig.height", {
  # früher: Höhe immer 5 (= Standardwert, daher bleibt die Standardausgabe gleich)
  reset_setanalysis_state()
  withr::local_dir(withr::local_tempdir())
  invisible(capture.output(merge_fachsem(lve$FachSemN, fig.height = 3)))

  svg <- paste(readLines(list.files("figure", full.names = TRUE)), collapse = "")
  expect_match(svg, "height='216.00pt'", fixed = TRUE) # 3 Zoll
})
