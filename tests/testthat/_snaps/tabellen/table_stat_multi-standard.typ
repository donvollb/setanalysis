#show figure: set block(breakable: false)
#figure( // start preamble figure
  
  kind: table, // end preamble figure

block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "1_0": 0, "3_0": 0, "5_0": 0, "7_0": 0, "9_0": 0, "11_0": 0, "13_0": 0, "15_0": 0, "1_1": 1, "3_1": 1, "5_1": 1, "7_1": 1, "9_1": 1, "11_1": 1, "13_1": 1, "15_1": 1, "1_2": 1, "3_2": 1, "5_2": 1, "7_2": 1, "9_2": 1, "11_2": 1, "13_2": 1, "15_2": 1, "1_3": 1, "3_3": 1, "5_3": 1, "7_3": 1, "9_3": 1, "11_3": 1, "13_3": 1, "15_3": 1, "1_4": 1, "3_4": 1, "5_4": 1, "7_4": 1, "9_4": 1, "11_4": 1, "13_4": 1, "15_4": 1, "1_5": 1, "3_5": 1, "5_5": 1, "7_5": 1, "9_5": 1, "11_5": 1, "13_5": 1, "15_5": 1, "1_6": 1, "3_6": 1, "5_6": 1, "7_6": 1, "9_6": 1, "11_6": 1, "13_6": 1, "15_6": 1, "2_0": 2, "4_0": 2, "6_0": 2, "8_0": 2, "10_0": 2, "12_0": 2, "14_0": 2, "2_1": 3, "4_1": 3, "6_1": 3, "8_1": 3, "10_1": 3, "12_1": 3, "14_1": 3, "2_2": 3, "4_2": 3, "6_2": 3, "8_2": 3, "10_2": 3, "12_2": 3, "14_2": 3, "2_3": 3, "4_3": 3, "6_3": 3, "8_3": 3, "10_3": 3, "12_3": 3, "14_3": 3, "2_4": 3, "4_4": 3, "6_4": 3, "8_4": 3, "10_4": 3, "12_4": 3, "14_4": 3, "2_5": 3, "4_5": 3, "6_5": 3, "8_5": 3, "10_5": 3, "12_5": 3, "14_5": 3, "2_6": 3, "4_6": 3, "6_6": 3, "8_6": 3, "10_6": 3, "12_6": 3, "14_6": 3, "0_0": 4, "0_1": 5, "0_2": 5, "0_3": 5, "0_4": 5, "0_5": 5, "0_6": 5
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: left,),
    (align: right,),
    (background: rgb("#6DACDC1A"), align: left,),
    (background: rgb("#6DACDC1A"), align: right,),
    (bold: true, background: rgb("#6DACDC1A"), align: left,),
    (bold: true, background: rgb("#6DACDC1A"), align: right,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  // tinytable align-figure before

  #table( // tinytable table start
    columns: (53.00%, 9.00%, 8.00%, 8.00%, 8.00%, 7.00%, 7.00%),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 7, stroke: 0.05em),
 table.hline(y: 16, start: 0, end: 7, stroke: 0.08em),
 table.hline(y: 0, start: 0, end: 7, stroke: 0.08em),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[Item], [N_votes], [M], [SD], [MD], [Min], [Max],
    ),
    // tinytable header end

    // tinytable cell content after
[Didaktische Hilfsmittel (z.B. Folien, Begleitmaterialien) waren für mich hilfreich.], [109], [4.93], [0.78], [5.17], [2.50], [6.00],
[Die Veranstaltung folgte aus meiner Sicht einer klaren Struktur.], [109], [5.21], [0.67], [5.33], [2.25], [6.00],
[Die Veranstaltung war meiner Ansicht nach gut organisiert (z.B. Bereitstellung von Materialien, Informationsfluss).], [109], [5.19], [0.75], [5.38], [2.40], [6.00],
[Der/Die Lehrende erklärte meiner Ansicht nach schwierige Sachverhalte verständlich.], [109], [5.26], [0.72], [5.42], [2.00], [6.00],
[Lernziele waren für mich transparent.], [109], [5.03], [0.77], [5.20], [2.60], [6.00],
[Ich finde, Leistungs- und Prüfungsanforderungen wurden transparent gemacht.], [109], [5.00], [0.68], [5.11], [2.80], [6.00],
[Die Veranstaltung regte mich zur Auseinandersetzung mit den Inhalten an.], [109], [5.00], [0.71], [5.00], [2.80], [6.00],
[Der/Die Lehrende verstand es mein Interesse am Thema zu wecken.], [109], [4.96], [0.76], [5.11], [2.00], [6.00],
[Der/Die Lehrende wirkte aus meiner Sicht im Umgang mit den Studierenden freundlich und aufgeschlossen.], [109], [5.65], [0.48], [5.75], [3.75], [6.00],
[Der/Die Lehrende ging in für mich angemessenem Umfang auf Fragen ein.], [109], [5.52], [0.62], [5.67], [2.00], [6.00],
[Die Interaktion mit der Lehrperson (z.B. Klären von Rückfragen, Erreichbarkeit) verlief problemlos.], [109], [5.51], [0.60], [5.71], [3.25], [6.00],
[Ich empfinde den von mir in dieser Veranstaltung zu erbringenden Arbeitsaufwand als angemessen.], [109], [5.08], [0.80], [5.23], [2.00], [6.00],
[Ich habe mich auf die einzelnen Veranstaltungstermine oder Themenblöcke regelmäßig vorbereitet oder diese nachbereitet.], [109], [4.69], [0.64], [4.75], [3.00], [5.83],
[Aus meiner Sicht wurde ein Bezug zwischen theoretischem Wissen und dessen Anwendung hergestellt.], [109], [4.95], [0.90], [5.17], [1.83], [6.00],
[Meiner Einschätzung nach wurde die Relevanz der behandelten Inhalte deutlich.], [109], [5.04], [0.69], [5.19], [2.67], [6.00],

    // tinytable footer after

  ) // end table

  // tinytable align-figure after

] // end block
) // end figure
