
```{=typst}
#show figure: set block(breakable: false)
#figure( // start preamble figure
  
  kind: table, // end preamble figure

block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "1_0": 0, "3_0": 0, "1_1": 1, "3_1": 1, "1_2": 1, "3_2": 1, "1_3": 1, "3_3": 1, "1_4": 1, "3_4": 1, "1_5": 1, "3_5": 1, "1_6": 1, "3_6": 1, "0_0": 2, "2_0": 2, "2_1": 3, "2_2": 3, "2_3": 3, "2_4": 3, "2_5": 3, "2_6": 3, "0_1": 4, "0_2": 4, "0_3": 4, "0_4": 4, "0_5": 4, "0_6": 4
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: left,),
    (align: right,),
    (background: rgb("#6DACDC1A"), align: left,),
    (background: rgb("#6DACDC1A"), align: right,),
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
 table.hline(y: 4, start: 0, end: 7, stroke: 0.08em),
 table.hline(y: 0, start: 0, end: 7, stroke: 0.08em),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[#text(weight: "bold")[Item] _[Skala: (1)~trifft gar nicht zu - (6)~trifft voll zu]_], [n], [M], [SD], [MD], [Min], [Max],
    ),
    // tinytable header end

    // tinytable cell content after
[Didaktische Hilfsmittel (z.B. Folien, Begleitmaterialien) waren für mich hilfreich.], [4420], [4.85], [1.10], [5], [1], [6],
[Die Veranstaltung folgte aus meiner Sicht einer klaren Struktur.], [4420], [5.01], [1.05], [5], [1], [6],
[Die Veranstaltung war meiner Ansicht nach gut organisiert (z.B. Bereitstellung von Materialien, Informationsfluss).], [4412], [4.93], [1.07], [5], [1], [6],

    // tinytable footer after

  ) // end table

  // tinytable align-figure after

] // end block
) // end figure
```<img src="figure/sub_chunk_2-1.svg" alt="" width="100%" style="display: block; margin: auto;" />  
  
###  Überschneidet sich der Termin dieser Lehrveranstaltung mit anderen laut Studienverlaufsplan (in diesem Semester) vorgesehenen Pflichtveranstaltungen? 
 

```{=typst}
#show figure: set block(breakable: false)
#figure( // start preamble figure
  
  kind: table, // end preamble figure

block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "1_0": 0, "3_0": 0, "1_1": 1, "3_1": 1, "1_2": 1, "3_2": 1, "1_3": 1, "3_3": 1, "2_0": 2, "4_0": 2, "2_1": 3, "4_1": 3, "2_2": 3, "4_2": 3, "2_3": 3, "4_3": 3, "0_0": 4, "0_1": 5, "0_2": 5, "0_3": 5
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
    columns: (65.00%, 14.00%, 8.00%, 13.00%),
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
 table.hline(y: 1, start: 0, end: 4, stroke: 0.05em),
 table.hline(y: 5, start: 0, end: 4, stroke: 0.08em),
 table.hline(y: 0, start: 0, end: 4, stroke: 0.08em),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[Antwortoption], [n], [%], [gültige %],
    ),
    // tinytable header end

    // tinytable cell content after
[ja], [230], [5.2], [5.2],
[nein], [4192], [94.0], [94.8],
[NAs], [39], [0.9], [NA],
[Total], [4461], [100.0], [100.0],

    // tinytable footer after

  ) // end table

  // tinytable align-figure after

] // end block
) // end figure
```<img src="figure/sub_chunk_4-1.svg" alt="" width="100%" style="display: block; margin: auto;" />
 
