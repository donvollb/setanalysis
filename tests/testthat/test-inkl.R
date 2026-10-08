# Die inkl.-Logik: Über das Argument `nr` wird eine globale Variable
# `inkl.<nr>` abgefragt (wie sie input_tabelle() pro Bericht erzeugt).
# Ist sie FALSE, gibt die Funktion nichts aus.

lve <- BspDaten$dataLVE
showup <- BspDaten$dataSHOWUP

test_that("inkl = FALSE oder inkl.<nr> = FALSE unterdrückt die Ausgabe", {
  local_inkl(`1.3` = FALSE, `1.19` = FALSE, `1.30` = FALSE, `1.34` = FALSE,
             `1.27` = FALSE, `1.1` = FALSE, `2.1` = FALSE)

  reset_setanalysis_state()
  expect_length(capture.output(merge_sc(lve$V3_D, nr = "1.3")), 0)
  expect_length(capture.output(merge_sc(lve$V3_D, inkl = FALSE)), 0)
  expect_length(capture.output(merge_fachsem(lve$FachSemN, nr = "1.19")), 0)
  expect_length(capture.output(merge_sk(showup$info_ausr_studgang, nr = "1.30")), 0)
  expect_length(capture.output(merge_open(showup$offen, nr = "1.34")), 0)
  expect_length(capture.output(merge_num(showup$zugang_note, nr = "1.27")), 0)
  expect_length(capture.output(
    merge_mc(showup[, paste0("abschluss_", 1:8)], nr = "1.1")), 0)
  expect_length(capture.output(merge_grade(lve$Note, lve$Kennung, nr = "2.1")), 0)
  expect_length(capture.output(
    merge_aggr_sk(lve[, c("KF_01", "KF_02")], nr = "2.1", inkl = FALSE)), 0)
})

test_that("merge_subj() braucht beide inkl.-Variablen", {
  local_inkl(`1.20` = TRUE, `1.21` = FALSE)
  reset_setanalysis_state()
  expect_length(capture.output(
    merge_subj(showup$fach1_2FB, showup$fach2_2FB, nr1 = "1.20", nr2 = "1.21")), 0)
})

test_that("inkl.<nr> = TRUE gibt die Frage mit Nummer aus", {
  local_inkl(`1.3` = TRUE, `1.20` = TRUE, `1.21` = TRUE)
  expect_report_snapshot(merge_sc(lve$V3_D, nr = "1.3"), "inkl-merge_sc-mit-nummer")
  expect_report_snapshot(
    merge_subj(showup$fach1_2FB, showup$fach2_2FB, nr1 = "1.20", nr2 = "1.21"),
    "inkl-merge_subj-mit-nummern"
  )
})

test_that("merge_aggr_sk() wählt Items einzeln über inkl.<header>.<k> aus", {
  local_inkl(`2.1` = TRUE, `2.2` = FALSE, `2.3` = TRUE)
  expect_report_snapshot(
    merge_aggr_sk(lve[, c("KF_01", "KF_02", "KF_03")], nr = "2.1"),
    "inkl-merge_aggr_sk-auswahl"
  )
})

test_that("merge_auto() und merge_many() ziehen die Nummer automatisch", {
  local_inkl(`1.1` = TRUE, `1.20` = FALSE, `1.21` = TRUE, `1.34` = TRUE, `1.27` = TRUE)
  expect_report_snapshot(merge_auto(lve$V3_D, inkl = TRUE), "inkl-merge_auto-nummer")
  expect_report_snapshot(merge_many(showup[, 1:12]), "inkl-merge_many-nummern")
})
