test_that("lv_table() formatiert Tabellen unverändert", {
  expect_table_snapshot(lv_table(head(mtcars, 5)), "lv_table-standard")
  expect_table_snapshot(
    lv_table(head(mtcars, 5), col.width = c(30, rep(7, 10)), bold = FALSE,
             digits = 1, striped = FALSE),
    "lv_table-optionen"
  )
  expect_table_snapshot(lv_table(head(mtcars, 1), bold.corner = FALSE),
                        "lv_table-eine-zeile")
})

test_that("table.freq() erzeugt unveränderte Häufigkeitstabellen", {
  expect_table_snapshot(table.freq(BspDaten$Tabellen$freq), "table.freq-standard")
  expect_table_snapshot(
    table.freq(BspDaten$Tabellen$freq, col1.name = "Antwort",
               order.table = "decreasing"),
    "table.freq-sortiert"
  )

  fachsem <- BspDaten$dataLVE$FachSemN
  fachsem[fachsem >= 12] <- 12
  expect_table_snapshot(
    table.freq(fachsem, cutoff = 12, show.all = FALSE, col1.name = "Fachsemester"),
    "table.freq-cutoff"
  )
})

test_that("table.stat.single() erzeugt unveränderte Statistiktabellen", {
  x <- BspDaten$dataLVE$KF_01
  expect_table_snapshot(table.stat.single(x), "table.stat.single-standard")
  expect_table_snapshot(table.stat.single(x, md = TRUE, col1.name = "n", digits = 1),
                        "table.stat.single-median")
})

test_that("table.stat.multi() erzeugt unveränderte Statistiktabellen", {
  multi <- BspDaten$Tabellen$multi
  expect_table_snapshot(table.stat.multi(multi), "table.stat.multi-standard")
  expect_table_snapshot(
    table.stat.multi(multi[, 1:3], col2.name = "n", bold.corner = FALSE,
                     alt1 = "weiß nicht", alt1.list = c(1, 2, 3),
                     alt2 = "trifft nicht zu", alt2.list = c(4, 5, 6)),
    "table.stat.multi-ausweichoptionen"
  )
  expect_error(
    table.stat.multi(multi, alt2 = "trifft nicht zu", alt2.list = 1:15),
    "alt1 ist FALSE"
  )
})

test_that("bsp.table.stat() erzeugt die Legenden-Tabellen unverändert", {
  expect_table_snapshot(bsp.table.stat(), "bsp.table.stat-alle")
  expect_table_snapshot(bsp.table.stat(all = FALSE), "bsp.table.stat-kurz")
})
