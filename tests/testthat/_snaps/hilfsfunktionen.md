# aggr_data() aggregiert unverändert

    structure(list(KF_01 = structure(c(5.42857142857143, 5.5, 4.5, 
    5.25, 4.42857142857143), label = "Didaktische Hilfsmittel (z.B. Folien, Begleitmaterialien) waren für mich hilfreich."), 
        KF_02 = structure(c(5.71428571428571, 5.66666666666667, 5.5, 
        5.25, 4.71428571428571), label = "Die Veranstaltung folgte aus meiner Sicht einer klaren Struktur.")), row.names = c(NA, 
    5L), class = "data.frame")

---

    structure(list(data.frame.vars..0... = structure(c(1.57142857142857, 
    1.5, 1.25, 1.5, 1.57142857142857), label = "Welche Gesamtnote (Schulnote) geben Sie der Veranstaltung insgesamt?")), row.names = c(NA, 
    5L), class = "data.frame")

# label.test() meldet Übereinstimmungen und Abweichungen

    Code
      label.test(BspDaten$pInfo$FB.txt, BspDaten$dataLVE$Teilbereich)
    Output
      [1] "Alle Labels der Spalte aus personalized.info kommen in gleicher Schreibweise auch in der Variable vor"
    Code
      label.test(BspDaten$pInfo$FB.txt.falsch, BspDaten$dataLVE$Teilbereich)
    Output
      [1] "Das Label \"Psüchologie - SoSe24\" aus der Spalte von personalized.info kommt nicht in gleicher Schreibweise in den Labels der Variable vor."
    Code
      label.test(BspDaten$pInfo$FB.txt.falsch, BspDaten$dataLVE$Teilbereich,
      exception = "Psüchologie - SoSe24")
    Output
      [1] "Alle Labels der Spalte aus personalized.info kommen in gleicher Schreibweise auch in der Variable vor"
    Code
      label.test(c("ja", "nein"), BspDaten$Tabellen$freq)
    Output
      [1] "Alle Labels der Spalte aus personalized.info kommen in gleicher Schreibweise auch in der Variable vor"

# Standardeinstellungen haben die erwarteten Werte

    Code
      str(as.list(setanalysis_defaults)[sort(ls(setanalysis_defaults))])
    Output
      List of 13
       $ col.width.sm     : num [1:7] 64 11 9 9 9 9 9
       $ col.width.sm.alt1: num [1:8] 59 8 8 8 8 8 8 15
       $ col.width.sm.alt2: num [1:9] 52 7 7 7 5 6 6 12 12
       $ col.width3       : num [1:3] 108 18 11
       $ col.width4       : num [1:4] 86 18 11 18
       $ col1.width.tss   : num 12
       $ color.bars       : chr "#6DACDC"
       $ font.family      : chr "Red Hat Text"
       $ inkl.open        : logi TRUE
       $ open.appendix    : logi TRUE
       $ show.plot.mc     : logi TRUE
       $ show.plot.sc     : logi TRUE
       $ show.plot.sk     : logi TRUE

