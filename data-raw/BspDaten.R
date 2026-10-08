# Erzeugt die fiktiven Beispieldaten BspDaten ------------------------------
#
# Alle Werte sind zufällig erzeugt (fester Seed, daher reproduzierbar).
# Fachbereiche und Fächer sind erfunden, offene Antworten bestehen aus
# Lorem ipsum. Aufbau, Spaltennamen und Attribute (label, nr, type, labels)
# entsprechen Daten aus evasys_read_data(), damit sich alle Funktionen des
# Pakets damit ausprobieren lassen.
#
# Die vorbereiteten Eingaben in `Plots` und `Tabellen` werden aus den
# simulierten Daten genauso berechnet wie in den Auswertungsfunktionen.
#
# Ausführen aus dem Paketverzeichnis: source("data-raw/BspDaten.R")

devtools::load_all(quiet = TRUE) # für aggr_data() und .wrap_labels()


# Hilfsfunktionen ---------------------------------------------------------

frage <- function(x, label, nr, type, labels = NULL) {
  attr(x, "label") <- label
  attr(x, "nr") <- nr
  attr(x, "type") <- type
  if (!is.null(labels)) attr(x, "labels") <- labels
  x
}

# Ganzzahlige Werte auf eine Skala begrenzen
begrenzen <- function(x, min, max) as.integer(pmin(pmax(round(x), min), max))

# Einen Anteil der Werte auf NA setzen (fehlende Angaben)
luecken <- function(x, anteil) {
  x[runif(length(x)) < anteil] <- NA
  x
}

lorem_woerter <- c(
  "lorem", "ipsum", "dolor", "sit", "amet", "consectetur", "adipiscing",
  "elit", "sed", "do", "eiusmod", "tempor", "incididunt", "ut", "labore",
  "et", "dolore", "magna", "aliqua", "enim", "ad", "minim", "veniam", "quis",
  "nostrud", "exercitation", "ullamco", "laboris", "nisi", "aliquip", "ex",
  "ea", "commodo", "consequat", "duis", "aute", "irure", "in",
  "reprehenderit", "voluptate", "velit", "esse", "cillum", "fugiat", "nulla",
  "pariatur", "excepteur", "sint", "occaecat", "cupidatat", "non",
  "proident", "sunt", "culpa", "qui", "officia", "deserunt", "mollit",
  "anim", "id", "est", "laborum"
)

lorem_satz <- function() {
  satz <- paste(sample(lorem_woerter, sample(4:14, 1), replace = TRUE), collapse = " ")
  paste0(toupper(substr(satz, 1, 1)), substr(satz, 2, nchar(satz)), ".")
}

lorem_antwort <- function() paste(replicate(sample(1:3, 1), lorem_satz()), collapse = " ")


# pInfo: Berichtstabelle der LVE (ein Bericht pro Fachbereich) -------------

fachbereiche <- c(
  "Musterwissenschaften", "Beispielkunde", "Platzhalterlehre", "Angewandte Fiktion"
)
teilbereiche <- paste(fachbereiche, "- SoSe24")

pInfo <- data.frame(
  pdf.no = seq_along(fachbereiche),
  pdf.name = paste("Fachbereich", fachbereiche, "LVE-Bericht SoSe24.pdf"),
  Auswahl = c("Basis", "Basis", "Basis", "Erweitert"),
  Stichprobe.txt = paste("Fachbereich", fachbereiche),
  FB.txt = teilbereiche,
  # absichtlicher Tippfehler zum Ausprobieren von label_test()
  FB.txt.falsch = c(teilbereiche[1:3], "Angewandte Fktion - SoSe24")
)


# dataLVE: Lehrveranstaltungsevaluation, eine Zeile pro Antwort -----------

set.seed(2024)

# Lehrveranstaltungen: Fachbereich, Kennung, Stimmen, Teilnehmende, Qualität
n_lv <- c(128, 110, 100, 111)
lv <- data.frame(
  Teilbereich = rep(teilbereiche, n_lv),
  Kennung = 1000 + seq_len(sum(n_lv)),
  qualitaet = rnorm(sum(n_lv), 0, 0.5)
)
lv$stimmen <- 3L + rnbinom(nrow(lv), size = 1.5, mu = 7) # mindestens 3 Stimmen
lv$Teilnehmer <- as.integer(pmax(lv$stimmen, round(lv$stimmen / rbeta(nrow(lv), 3, 7))))
# Bei einigen Veranstaltungen wurde die Teilnehmendenzahl zu niedrig angegeben
# (Rücklauf über 100 %)
zu_niedrig <- sample(nrow(lv), 6)
lv$Teilnehmer[zu_niedrig] <- as.integer(pmax(1, round(lv$stimmen[zu_niedrig] / c(1.2, 1.5, 2, 3, 5, 7))))

dataLVE <- lv[rep(seq_len(nrow(lv)), lv$stimmen), c("Teilbereich", "Kennung", "Teilnehmer", "qualitaet")]
n <- nrow(dataLVE)

skala_6 <- setNames(1:6, c("trifft gar nicht zu", "", "", "", "", "trifft voll zu"))
kernfrage <- function(verschiebung) {
  luecken(begrenzen(4.9 + verschiebung + dataLVE$qualitaet + rnorm(n, 0, 1.2), 1, 6), 0.009)
}

dataLVE$FachSemN <- frage(
  luecken(sample(1:20, n,
    replace = TRUE,
    prob = c(9, 36, 6, 26, 3, 9, 2, 4, 1, 2, 0.6, 0.3, 0.3, 0.2, 0.1, 0.05, 0.05, 0.05, 0, 0.1)
  ), 0.02),
  label = "Bezogen auf das Fach, dem die vorliegende Veranstaltung zugehört: In welchem Fachsemester sind Sie eingeschrieben?",
  nr = "1.19", type = "sc",
  labels = setNames(1:20, c(sprintf("%02d", 1:19), "20 und höher"))
)
dataLVE$KF_01 <- frage(
  kernfrage(0.1),
  label = "Didaktische Hilfsmittel (z.B. Folien, Begleitmaterialien) waren für mich hilfreich.",
  nr = "2.1", type = "sk", labels = skala_6
)
dataLVE$KF_02 <- frage(
  kernfrage(0.3),
  label = "Die Veranstaltung folgte aus meiner Sicht einer klaren Struktur.",
  nr = "2.2", type = "sk", labels = skala_6
)
dataLVE$KF_03 <- frage(
  kernfrage(0.2),
  label = "Die Veranstaltung war meiner Ansicht nach gut organisiert (z.B. Bereitstellung von Materialien, Informationsfluss).",
  nr = "2.3", type = "sk", labels = skala_6
)
dataLVE$Note <- frage(
  luecken(begrenzen(2 - 0.9 * dataLVE$qualitaet + rnorm(n, 0, 0.95), 1, 6), 0.009),
  label = "Welche Gesamtnote (Schulnote) geben Sie der Veranstaltung insgesamt?",
  nr = "2.1", type = "sk",
  labels = c(
    "sehr gut" = 1, "gut" = 2, "befriedigend" = 3, "ausreichend" = 4,
    "mangelhaft" = 5, "ungenügend" = 6
  )
)
dataLVE$V3_D <- frage(
  luecken(sample(1:2, n, replace = TRUE, prob = c(0.05, 0.95)), 0.008),
  label = "Überschneidet sich der Termin dieser Lehrveranstaltung mit anderen laut Studienverlaufsplan (in diesem Semester) vorgesehenen Pflichtveranstaltungen?",
  nr = "1.3", type = "sc", labels = c("ja" = 1, "nein" = 2)
)
dataLVE$WL <- frage(
  luecken(as.integer(pmin(round(rgamma(n, shape = 2, scale = 1.1)), 13) + 1), 0.04), # Code 1 = 0 Stunden
  label = "Zusätzlich zu Ihren Anwesenheitszeiten in der Veranstaltung: Wie viel Zeit (in Zeitstunden) haben Sie für die vorliegende Veranstaltung im Schnitt pro Woche aufgewendet? (ohne Prüfungsvorbereitung)",
  nr = "1.5", type = "sc", labels = setNames(1:14, c(0:12, "mehr als 12"))
)

dataLVE <- dataLVE[c(
  "Teilbereich", "FachSemN", "Kennung", "KF_01", "KF_02", "KF_03",
  "Teilnehmer", "Note", "V3_D", "WL"
)]
rownames(dataLVE) <- NULL


# dataSHOWUP: Studieneingangsbefragung ------------------------------------

set.seed(2025)
n <- 242

abschluesse <- c(
  "Bachelor of Arts (B.A.)", "Bachelor of Education (B.Ed.)",
  "Bachelor of Science (B.Sc.)", "2-Fach-Bachelor (B.A., B.Sc.)",
  "Master of Arts (M.A.)", "Master of Education (M.Ed.)",
  "Master of Science (M.Sc.)", "lehramtsbezogener Zertifikatsstudiengang"
)
abschluss <- sample(1:7, n, replace = TRUE, prob = c(0.07, 0.29, 0.24, 0.05, 0.05, 0.1, 0.2))
bachelor <- abschluss <= 4

dataSHOWUP <- data.frame(row.names = seq_len(n))

# Multiple Choice: eine Spalte je Option, Wert = Code der Option oder 0
for (k in seq_along(abschluesse)) {
  dataSHOWUP[[paste0("abschluss_", k)]] <- frage(
    ifelse(abschluss == k, k, 0L),
    label = paste(
      "Welchen Studienabschluss streben Sie an? (Mehrfachnennung möglich) :",
      abschluesse[k]
    ),
    nr = "1.1", type = "mc"
  )
}

faecher <- c(
  "Allgemeine Musterwissenschaft", "Beispielkunde", "Platzhalterlehre/ Vorlagenkunde",
  "Angewandte Fiktion", "Fantasie-Studien", "Geographie: Musterlandschaften",
  "Testologie", "Theorie der Beispiele", "Kunst und Platzhalter", "Mathematik",
  "Ökologie", "Philosophie", "Physik", "Politikwissenschaft", "Soziologie",
  "Sportwissenschaft", "Umweltchemie", "Wirtschaftswissenschaft"
)
zfb <- abschluss == 4
fach1 <- rep(NA_integer_, n)
fach2 <- rep(NA_integer_, n)
fach1[zfb] <- sample(c(1, 2, 7, 11, 12, 18), sum(zfb), replace = TRUE)
fach2[zfb] <- vapply(fach1[zfb], \(f) {
  as.integer(sample(setdiff(c(3, 6, 9, 12, 14), f), 1))
}, integer(1))

dataSHOWUP$fach1_2FB <- frage(
  as.integer(fach1),
  label = "[ZFB] Der 2-Fach-Bachelor kombiniert 2 Basisfächer: Was ist Ihr 1.Fach?",
  nr = "1.20", type = "sc", labels = setNames(seq_along(faecher), faecher)
)
dataSHOWUP$fach2_2FB <- frage(
  fach2,
  label = "[ZFB] Der 2-Fach-Bachelor kombiniert 2 Basisfächer: Was ist Ihr 2.Fach?",
  nr = "1.21", type = "sc", labels = setNames(seq_along(faecher), faecher)
)

# Offene Antworten: Lorem ipsum, einige Antworten mehrfach (auch in anderer
# Groß-/Kleinschreibung), damit merge_open() Häufigkeiten zeigt
offen <- rep(NA_character_, n)
antwortende <- sample(n, 26)
offen[antwortende] <- replicate(length(antwortende), lorem_antwort())
offen[antwortende[1:3]] <- "Lorem ipsum dolor sit amet."
offen[antwortende[4]] <- "lorem ipsum dolor sit amet."
offen[antwortende[5:6]] <- "Sed do eiusmod tempor."
dataSHOWUP$offen <- frage(
  offen,
  label = "[FILTER_123] Welche weiteren Informationen hätten Sie gerne gehabt, damit Sie sich vor Beginn Ihres Studiums ausreichend über den Studiengang informiert gefühlt hätten?",
  nr = "1.34", type = "open/num"
)

dataSHOWUP$zugang_note <- frage(
  luecken(ifelse(bachelor, round(pmin(pmax(rnorm(n, 2.2, 0.55), 1), 4), 1), NA), 0.03),
  label = "[BACHELOR] Welche Durchschnittsnote hatten Sie in dem Zeugnis, mit dem Sie Ihre Hochschulzugangsberechtigung erworben haben?",
  nr = "1.27", type = "open/num"
)

dataSHOWUP$info_ausr_studgang <- frage(
  luecken(sample(0:6, n, replace = TRUE, prob = c(0.02, 0.03, 0.08, 0.09, 0.24, 0.36, 0.18)), 0.01),
  label = "Vor Beginn meines Studiums war ich ausreichend über den Studiengang informiert.",
  nr = "1.30", type = "sk",
  labels = c(
    "stimme gar nicht zu" = 1, "stimme nicht zu" = 2, "stimme eher nicht zu" = 3,
    "stimme eher zu" = 4, "stimme zu" = 5, "stimme voll zu" = 6,
    "kann ich nicht beurteilen" = 0
  )
)


# Tabellen ----------------------------------------------------------------

# multi: Mittelwerte von 15 Items je Lehrveranstaltung (109 Veranstaltungen)
set.seed(2026)
items <- c(
  KF_01 = "Didaktische Hilfsmittel (z.B. Folien, Begleitmaterialien) waren für mich hilfreich.",
  KF_02 = "Die Veranstaltung folgte aus meiner Sicht einer klaren Struktur.",
  KF_03 = "Die Veranstaltung war meiner Ansicht nach gut organisiert (z.B. Bereitstellung von Materialien, Informationsfluss).",
  KFz_3_1N = "Der/Die Lehrende erklärte meiner Ansicht nach schwierige Sachverhalte verständlich.",
  KF_04 = "Lernziele waren für mich transparent.",
  KFz_4_1N = "Ich finde, Leistungs- und Prüfungsanforderungen wurden transparent gemacht.",
  KF_05 = "Die Veranstaltung regte mich zur Auseinandersetzung mit den Inhalten an.",
  KF_06NN = "Der/Die Lehrende verstand es mein Interesse am Thema zu wecken.",
  KF_07 = "Der/Die Lehrende wirkte aus meiner Sicht im Umgang mit den Studierenden freundlich und aufgeschlossen.",
  KF_08 = "Der/Die Lehrende ging in für mich angemessenem Umfang auf Fragen ein.",
  KFz_8_2N = "Die Interaktion mit der Lehrperson (z.B. Klären von Rückfragen, Erreichbarkeit) verlief problemlos.",
  KFz_4_4NN = "Ich empfinde den von mir in dieser Veranstaltung zu erbringenden Arbeitsaufwand als angemessen.",
  KFz_4_3N = "Ich habe mich auf die einzelnen Veranstaltungstermine oder Themenblöcke regelmäßig vorbereitet oder diese nachbereitet.",
  KFz_3_2 = "Aus meiner Sicht wurde ein Bezug zwischen theoretischem Wissen und dessen Anwendung hergestellt.",
  ap_rel = "Meiner Einschätzung nach wurde die Relevanz der behandelten Inhalte deutlich."
)
n_multi <- 109
stimmen <- 3L + rnbinom(n_multi, size = 1.5, mu = 6)
kennung_multi <- rep(seq_len(n_multi), stimmen)
qualitaet <- rep(rnorm(n_multi, 0, 0.45), stimmen)
lage <- c(0.2, 0.4, 0.3, 0.2, 0.1, 0, 0, -0.1, 0.6, 0.4, 0.3, -0.2, -0.9, 0.1, 0.3)
antworten <- data.frame(lapply(seq_along(items), \(k) {
  frage(
    begrenzen(4.9 + lage[k] + qualitaet + rnorm(length(qualitaet), 0, 1), 1, 6),
    label = unname(items[k]), nr = paste0("2.", k), type = "sk", labels = skala_6
  )
}))
names(antworten) <- names(items)
multi <- aggr_data(antworten, kennung_multi)

Tabellen <- list(multi = multi)

# freq: Faktor ja/nein (aus V3_D)
v3 <- dataLVE$V3_D
freq <- factor(names(attr(v3, "labels"))[match(v3, attr(v3, "labels"))], levels = c("ja", "nein"))
attr(freq, "label") <- attr(v3, "label")
Tabellen$freq <- freq


# Plots: vorbereitete Eingaben für die Grafikfunktionen -------------------

# Mittelwerte je Lehrveranstaltung (wie merge_aggr_sk(), Items von unten
# nach oben) mit umbrochenen Beschriftungen und Skalenbeschriftung
aggr.data <- multi[, rev(seq_len(ncol(multi)))]
aggr.labels <- .wrap_labels(vapply(aggr.data, attr, character(1), which = "label"), width = 47)
names(aggr.labels) <- vapply(aggr.data, attr, character(1), which = "label")
aggr.skala <- c("trifft gar nicht zu", "", "", "", "", "trifft voll zu")

# Workload: Median je Lehrveranstaltung (wie merge_wl())
kennung <- dataLVE$Kennung
WL <- vapply(unique(kennung), \(k) median(dataLVE$WL[kennung == k], na.rm = TRUE), numeric(1))

# Gesamtnote: Mittelwert je Lehrveranstaltung (wie merge_grade())
grade <- aggr_data(dataLVE$Note, kennung)

# Rücklauf in Prozent je Lehrveranstaltung (wie merge_rueck())
per_course <- dataLVE[!duplicated(kennung), c("Kennung", "Teilnehmer")]
merged <- merge(
  data.frame(kennung = per_course$Kennung, x = per_course$Teilnehmer),
  data.frame(table(kennung)),
  by = "kennung"
)
rueck <- as.numeric(merged$Freq) / as.numeric(merged$x) * 100

# Durchschnittsnote in Klassen (für barplot_freq())
num <- cut(as.numeric(dataSHOWUP$zugang_note),
  breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4.01), right = FALSE,
  labels = c("1,0 bis 1,4", "1,5 bis 1,9", "2,0 bis 2,4", "2,5 bis 2,9", "3,0 bis 3,4", "3,5 bis 4,0")
)

# Fachsemester, ab 12 zusammengefasst (wie merge_fachsem())
fachsem <- dataLVE$FachSemN
fsem <- factor(ifelse(fachsem >= 12, "12+", as.character(fachsem)), levels = c(1:11, "12+"))

# Häufigkeiten einer SC-Frage (wie merge_sc(), Prozent bezogen auf alle)
sc_counts <- c(ja = sum(v3 == 1, na.rm = TRUE), nein = sum(v3 == 2, na.rm = TRUE))
sc <- data.frame(
  label = names(sc_counts), freq = as.numeric(sc_counts),
  perc = round(sc_counts / length(v3) * 100, 2), row.names = names(sc_counts)
)

# Häufigkeiten einer MC-Frage (wie merge_mc(), ohne NAs/Total)
mc_spalten <- dataSHOWUP[paste0("abschluss_", seq_along(abschluesse))]
mc_counts <- vapply(mc_spalten, \(x) sum(x != 0, na.rm = TRUE), numeric(1))
mc <- data.frame(
  label = unname(.wrap_labels(abschluesse, width = 40)),
  freq = unname(mc_counts),
  perc = unname(mc_counts) / nrow(mc_spalten) * 100
)

Plots <- list(
  aggr.data = aggr.data, aggr.labels = aggr.labels, aggr.skala = aggr.skala,
  WL = WL, grade = grade, rueck = rueck, num = num, fsem = fsem, sc = sc, mc = mc
)


# Speichern ---------------------------------------------------------------

BspDaten <- list(
  pInfo = pInfo, dataLVE = dataLVE, dataSHOWUP = dataSHOWUP,
  Tabellen = Tabellen, Plots = Plots
)
usethis::use_data(BspDaten, overwrite = TRUE, compress = "xz")
