# input_tabelle() erzeugt die inkl.- und header-Spalten unverändert

    Code
      print(as.data.frame(berichte))
    Output
                            Code          Art                   Abschluss
      1                   MASTER alles.master                        alle
      2            Gesamtbericht        alles                        alle
      3 B.Sc._Musterwissenschaft  Studiengang Bachelor of Science (B.Sc.)
      4 M.Sc._Musterwissenschaft  Studiengang   Master of Science (M.Sc.)
      5       B.A._Beispielkunde  Studiengang     Bachelor of Arts (B.A.)
      6         Sonderauswertung     speziell                        alle
               Studiengang                                    Titel inkl.1.1 inkl.1.2
      1               alle                   Befragung 2024: Master     TRUE     TRUE
      2               alle            Befragung 2024: Gesamtbericht     TRUE     TRUE
      3 Musterwissenschaft Befragung 2024: B.Sc. Musterwissenschaft     TRUE     TRUE
      4 Musterwissenschaft Befragung 2024: M.Sc. Musterwissenschaft     TRUE     TRUE
      5      Beispielkunde       Befragung 2024: B.A. Beispielkunde     TRUE     TRUE
      6               alle         Befragung 2024: Sonderauswertung     TRUE    FALSE
        inkl.1.3 inkl.2.1 inkl.2.2 inkl.3.1 header1 header2 header3
      1     TRUE     TRUE     TRUE     TRUE    TRUE    TRUE    TRUE
      2     TRUE    FALSE    FALSE    FALSE    TRUE   FALSE   FALSE
      3     TRUE     TRUE    FALSE    FALSE    TRUE    TRUE   FALSE
      4    FALSE     TRUE    FALSE    FALSE    TRUE    TRUE   FALSE
      5    FALSE    FALSE    FALSE    FALSE    TRUE   FALSE   FALSE
      6    FALSE    FALSE    FALSE     TRUE    TRUE   FALSE    TRUE

