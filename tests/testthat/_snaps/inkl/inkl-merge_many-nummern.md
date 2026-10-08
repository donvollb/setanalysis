###  1.1 Welchen Studienabschluss streben Sie an? (Mehrfachnennung möglich)  
 

```{=typst}
#show figure: set block(breakable: false)
#figure( // start preamble figure
  
  kind: table, // end preamble figure

block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "1_0": 0, "3_0": 0, "5_0": 0, "7_0": 0, "9_0": 0, "1_1": 1, "3_1": 1, "5_1": 1, "7_1": 1, "9_1": 1, "1_2": 1, "3_2": 1, "5_2": 1, "7_2": 1, "9_2": 1, "1_3": 1, "3_3": 1, "5_3": 1, "7_3": 1, "9_3": 1, "2_0": 2, "4_0": 2, "6_0": 2, "8_0": 2, "10_0": 2, "2_1": 3, "4_1": 3, "6_1": 3, "8_1": 3, "10_1": 3, "2_2": 3, "4_2": 3, "6_2": 3, "8_2": 3, "10_2": 3, "2_3": 3, "4_3": 3, "6_3": 3, "8_3": 3, "10_3": 3, "0_0": 4, "0_1": 5, "0_2": 5, "0_3": 5
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
 table.hline(y: 11, start: 0, end: 4, stroke: 0.08em),
 table.hline(y: 0, start: 0, end: 4, stroke: 0.08em),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[Antwortoption], [n], [%], [gültige %],
    ),
    // tinytable header end

    // tinytable cell content after
[Bachelor of Arts (B.A.)], [21], [8.7], [8.7],
[Bachelor of Education (B.Ed.)], [71], [29.3], [29.3],
[Bachelor of Science (B.Sc.)], [52], [21.5], [21.5],
[2\-Fach\-Bachelor (B.A., B.Sc.)], [12], [5.0], [5.0],
[Master of Arts (M.A.)], [18], [7.4], [7.4],
[Master of Education (M.Ed.)], [26], [10.7], [10.7],
[Master of Science (M.Sc.)], [42], [17.4], [17.4],
[lehramtsbezogener Zertifikatsstudiengang], [0], [0.0], [0.0],
[NAs], [0], [0.0], [NA],
[Total], [242], [NA], [NA],

    // tinytable footer after

  ) // end table

  // tinytable align-figure after

] // end block
) // end figure
```<img src="figure/sub_chunk_2-1.svg" alt="" width="100%" style="display: block; margin: auto;" />  
  
### 1.21 [ZFB] Der 2-Fach-Bachelor kombiniert 2 Basisfächer: Was ist Ihr 2.Fach? 
 

```{=typst}
#show figure: set block(breakable: false)
#figure( // start preamble figure
  
  kind: table, // end preamble figure

block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "1_0": 0, "3_0": 0, "5_0": 0, "7_0": 0, "9_0": 0, "11_0": 0, "13_0": 0, "15_0": 0, "17_0": 0, "19_0": 0, "1_1": 1, "3_1": 1, "5_1": 1, "7_1": 1, "9_1": 1, "11_1": 1, "13_1": 1, "15_1": 1, "17_1": 1, "19_1": 1, "1_2": 1, "3_2": 1, "5_2": 1, "7_2": 1, "9_2": 1, "11_2": 1, "13_2": 1, "15_2": 1, "17_2": 1, "19_2": 1, "1_3": 1, "3_3": 1, "5_3": 1, "7_3": 1, "9_3": 1, "11_3": 1, "13_3": 1, "15_3": 1, "17_3": 1, "19_3": 1, "2_0": 2, "4_0": 2, "6_0": 2, "8_0": 2, "10_0": 2, "12_0": 2, "14_0": 2, "16_0": 2, "18_0": 2, "20_0": 2, "2_1": 3, "4_1": 3, "6_1": 3, "8_1": 3, "10_1": 3, "12_1": 3, "14_1": 3, "16_1": 3, "18_1": 3, "20_1": 3, "2_2": 3, "4_2": 3, "6_2": 3, "8_2": 3, "10_2": 3, "12_2": 3, "14_2": 3, "16_2": 3, "18_2": 3, "20_2": 3, "2_3": 3, "4_3": 3, "6_3": 3, "8_3": 3, "10_3": 3, "12_3": 3, "14_3": 3, "16_3": 3, "18_3": 3, "20_3": 3, "0_0": 4, "0_1": 5, "0_2": 5, "0_3": 5
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
 table.hline(y: 21, start: 0, end: 4, stroke: 0.08em),
 table.hline(y: 0, start: 0, end: 4, stroke: 0.08em),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[Antwortoption], [n], [%], [gültige %],
    ),
    // tinytable header end

    // tinytable cell content after
[Allgemeine Musterwissenschaft], [0], [0.0], [0.0],
[Beispielkunde], [0], [0.0], [0.0],
[Platzhalterlehre\/ Vorlagenkunde], [3], [1.2], [25.0],
[Angewandte Fiktion], [0], [0.0], [0.0],
[Fantasie\-Studien], [0], [0.0], [0.0],
[Geographie: Musterlandschaften], [3], [1.2], [25.0],
[Testologie], [0], [0.0], [0.0],
[Theorie der Beispiele], [0], [0.0], [0.0],
[Kunst und Platzhalter], [4], [1.7], [33.3],
[Mathematik], [0], [0.0], [0.0],
[Ökologie], [0], [0.0], [0.0],
[Philosophie], [1], [0.4], [8.3],
[Physik], [0], [0.0], [0.0],
[Politikwissenschaft], [1], [0.4], [8.3],
[Soziologie], [0], [0.0], [0.0],
[Sportwissenschaft], [0], [0.0], [0.0],
[Umweltchemie], [0], [0.0], [0.0],
[Wirtschaftswissenschaft], [0], [0.0], [0.0],
[NAs], [230], [95.0], [NA],
[Total], [242], [100.0], [100.0],

    // tinytable footer after

  ) // end table

  // tinytable align-figure after

] // end block
) // end figure
```<img src="figure/sub_chunk_4-1.svg" alt="" width="100%" style="display: block; margin: auto;" />
 
### 1.34 [FILTER_123] Welche weiteren Informationen hätten Sie gerne gehabt, damit Sie sich vor Beginn Ihres Studiums ausreichend über den Studiengang informiert gefühlt hätten? {#sec-1.top} 

*Die offenen Antworten zu dieser Frage finden sich* [im Anhang](#sec-1.bottom).  

\

::: {.block breakable=false}

###  1.27   [BACHELOR] Welche Durchschnittsnote hatten Sie in dem Zeugnis, mit dem Sie Ihre Hochschulzugangsberechtigung erworben haben? 
 

```{=typst}
#show figure: set block(breakable: false)
#figure( // start preamble figure
  
  kind: table, // end preamble figure

block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "1_1": 0, "1_2": 0, "1_3": 0, "1_4": 0, "1_5": 0, "1_0": 1, "0_1": 2, "0_2": 2, "0_3": 2, "0_4": 2, "0_5": 2, "0_0": 3
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: center,),
    (align: left,),
    (bold: true, background: rgb("#6DACDC1A"), align: center,),
    (bold: true, background: rgb("#6DACDC1A"), align: left,),
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
    columns: (8.33%, 8.33%, 8.33%, 8.33%, 8.33%, 8.33%),
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
 table.hline(y: 1, start: 0, end: 6, stroke: 0.05em),
 table.hline(y: 2, start: 0, end: 6, stroke: 0.08em),
 table.hline(y: 0, start: 0, end: 6, stroke: 0.08em),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[n], [M], [SD], [MD], [Min], [Max],
    ),
    // tinytable header end

    // tinytable cell content after
[147], [2.19], [0.56], [2.10], [1], [3.80],

    // tinytable footer after

  ) // end table

  // tinytable align-figure after

] // end block
) // end figure
```  
  
<img src="figure/sub_chunk_6-1.svg" alt="" width="100%" style="display: block; margin: auto;" />

```
## Error in `seq.default()`:
## ! wrong sign in 'by' argument
```

:::

