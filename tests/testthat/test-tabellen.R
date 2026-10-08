test_that("lv_table() formatiert Tabellen unverändert", {
  expect_table_snapshot(lv_table(head(mtcars, 5)), "lv_table-standard")
  expect_table_snapshot(
    lv_table(head(mtcars, 5),
      col.width = c(30, rep(7, 10)), bold = FALSE,
      digits = 1, striped = FALSE
    ),
    "lv_table-optionen"
  )
  expect_table_snapshot(
    lv_table(head(mtcars, 1), bold.corner = FALSE),
    "lv_table-eine-zeile"
  )
})

# Sonderzeichen, die in Typst-Markup eine Bedeutung haben
sonderzeichen <- c(
  "a // b", "kostet $5", "#wort", "ein _ x", "x*y", "<label>", "@verweis",
  "back\\slash", "[Klammer]", "- Strich", "+ plus", "= Titel", "~ tilde",
  "`code`", "\"Zitat\""
)

typst_zeilen <- function(tabelle) {
  strsplit(tinytable::save_tt(tabelle, output = "typst"), "\n")[[1]]
}

test_that("lv_table() maskiert Sonderzeichen in den Zellen, nicht im Kopf", {
  kopf <- '#text(weight: "bold")[Item] _[Skala: a]_'
  daten <- data.frame(sonderzeichen, seq_along(sonderzeichen))
  names(daten) <- c(kopf, "n")
  zeilen <- typst_zeilen(lv_table(daten))

  # Kopfzeile bleibt Typst-Code
  expect_true(any(grepl(kopf, zeilen, fixed = TRUE)))
  # Zellen: jede Antwort steht maskiert in der Tabelle
  maskiert <- c(
    "a \\/\\/ b", "kostet \\$5", "\\#wort", "ein \\_ x", "x\\*y",
    "\\<label\\>", "\\@verweis", "back\\\\slash", "\\[Klammer\\]",
    "\\- Strich", "\\+ plus", "\\= Titel", "\\~ tilde", "\\`code\\`",
    "\\\"Zitat\\\""
  )
  for (k in seq_along(maskiert)) {
    expect_true(any(grepl(paste0("[", maskiert[k], "]"), zeilen, fixed = TRUE)),
      label = sonderzeichen[k]
    )
  }
})

test_that(".escape_typst() maskiert wie tinytable", {
  tab <- lv_table(data.frame(Antwort = sonderzeichen))
  zeilen <- typst_zeilen(tab)
  for (zeichen in sonderzeichen) {
    expect_true(
      any(grepl(paste0("[", .escape_typst(zeichen), "]"), zeilen, fixed = TRUE)),
      label = zeichen
    )
  }
  expect_identical(.escape_typst("trifft gar nicht zu"), "trifft gar nicht zu")
})

test_that("merge_aggr_sk() maskiert Skalenlabels im Tabellenkopf", {
  withr::local_dir(withr::local_tempdir())
  items <- BspDaten$dataLVE[, c("KF_01", "KF_02")]
  ausgabe <- capture.output(
    merge_aggr_sk(items, tmin = "nie_selten", tmax = "*immer*", show.plot = FALSE)
  )
  expect_true(any(grepl(
    "_[Skala: (1)~nie\\_selten - (6)~\\*immer\\*]_", ausgabe,
    fixed = TRUE
  )))
})

test_that("Tabellen mit Sonderzeichen lassen sich mit Typst kompilieren", {
  skip_on_cran()
  quarto <- Sys.which("quarto")
  skip_if(!nzchar(quarto), "Quarto ist nicht installiert")

  daten <- data.frame(sonderzeichen, seq_along(sonderzeichen))
  names(daten) <- c('#text(weight: "bold")[Item] _[Skala: (1)~a - (5)~b]_', "n")
  dir <- withr::local_tempdir()
  typ <- file.path(dir, "tabelle.typ")
  writeLines(tinytable::save_tt(lv_table(daten, bold.corner = FALSE), output = "typst"), typ)

  ergebnis <- suppressWarnings(system2(
    quarto, c("typst", "compile", shQuote(typ), shQuote(file.path(dir, "tabelle.pdf"))),
    stdout = TRUE, stderr = TRUE
  ))
  expect_null(attr(ergebnis, "status"), label = paste(ergebnis, collapse = "\n"))
  expect_true(file.exists(file.path(dir, "tabelle.pdf")))
})

test_that("table_freq() erzeugt unveränderte Häufigkeitstabellen", {
  expect_table_snapshot(table_freq(BspDaten$Tabellen$freq), "table_freq-standard")
  expect_table_snapshot(
    table_freq(BspDaten$Tabellen$freq,
      col1.name = "Antwort",
      order.table = "decreasing"
    ),
    "table_freq-sortiert"
  )

  fachsem <- BspDaten$dataLVE$FachSemN
  fachsem[fachsem >= 12] <- 12
  expect_table_snapshot(
    table_freq(fachsem, cutoff = 12, show.all = FALSE, col1.name = "Fachsemester"),
    "table_freq-cutoff"
  )
})

test_that("table_stat_single() erzeugt unveränderte Statistiktabellen", {
  x <- BspDaten$dataLVE$KF_01
  expect_table_snapshot(table_stat_single(x), "table_stat_single-standard")
  expect_table_snapshot(
    table_stat_single(x, md = TRUE, col1.name = "n", digits = 1),
    "table_stat_single-median"
  )
})

test_that("table_stat_multi() erzeugt unveränderte Statistiktabellen", {
  multi <- BspDaten$Tabellen$multi
  expect_table_snapshot(table_stat_multi(multi), "table_stat_multi-standard")
  expect_table_snapshot(
    table_stat_multi(multi[, 1:3],
      col2.name = "n", bold.corner = FALSE,
      alt1 = "weiß nicht", alt1.list = c(1, 2, 3),
      alt2 = "trifft nicht zu", alt2.list = c(4, 5, 6)
    ),
    "table_stat_multi-ausweichoptionen"
  )
  expect_error(
    table_stat_multi(multi, alt2 = "trifft nicht zu", alt2.list = 1:15),
    "alt1 ist FALSE"
  )
})

test_that("bsp_table_stat() erzeugt die Legenden-Tabellen unverändert", {
  expect_table_snapshot(bsp_table_stat(), "bsp_table_stat-alle")
  expect_table_snapshot(bsp_table_stat(all = FALSE), "bsp_table_stat-kurz")
})
