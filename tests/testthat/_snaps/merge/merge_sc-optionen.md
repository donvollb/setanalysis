###  Bezogen auf das Fach, dem die vorliegende Veranstaltung zugehört: In welchem Fachsemester sind Sie eingeschrieben? 
 

```{=typst}
#show figure: set block(breakable: false)
#figure( // start preamble figure
  
  kind: table, // end preamble figure

block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "1_0": 0, "3_0": 0, "5_0": 0, "7_0": 0, "9_0": 0, "11_0": 0, "13_0": 0, "15_0": 0, "17_0": 0, "19_0": 0, "21_0": 0, "1_1": 1, "3_1": 1, "5_1": 1, "7_1": 1, "9_1": 1, "11_1": 1, "13_1": 1, "15_1": 1, "17_1": 1, "19_1": 1, "21_1": 1, "1_2": 1, "3_2": 1, "5_2": 1, "7_2": 1, "9_2": 1, "11_2": 1, "13_2": 1, "15_2": 1, "17_2": 1, "19_2": 1, "21_2": 1, "1_3": 1, "3_3": 1, "5_3": 1, "7_3": 1, "9_3": 1, "11_3": 1, "13_3": 1, "15_3": 1, "17_3": 1, "19_3": 1, "21_3": 1, "2_0": 2, "4_0": 2, "6_0": 2, "8_0": 2, "10_0": 2, "12_0": 2, "14_0": 2, "16_0": 2, "18_0": 2, "20_0": 2, "22_0": 2, "2_1": 3, "4_1": 3, "6_1": 3, "8_1": 3, "10_1": 3, "12_1": 3, "14_1": 3, "16_1": 3, "18_1": 3, "20_1": 3, "22_1": 3, "2_2": 3, "4_2": 3, "6_2": 3, "8_2": 3, "10_2": 3, "12_2": 3, "14_2": 3, "16_2": 3, "18_2": 3, "20_2": 3, "22_2": 3, "2_3": 3, "4_3": 3, "6_3": 3, "8_3": 3, "10_3": 3, "12_3": 3, "14_3": 3, "16_3": 3, "18_3": 3, "20_3": 3, "22_3": 3, "0_0": 4, "0_1": 5, "0_2": 5, "0_3": 5
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
 table.hline(y: 23, start: 0, end: 4, stroke: 0.08em),
 table.hline(y: 0, start: 0, end: 4, stroke: 0.08em),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[Antwortoption], [Anzahl], [%], [gültige %],
    ),
    // tinytable header end

    // tinytable cell content after
[02], [1599], [35.84], [36.60],
[04], [1130], [25.33], [25.86],
[01], [399], [8.94], [9.13],
[06], [395], [8.85], [9.04],
[03], [262], [5.87], [6.00],
[08], [162], [3.63], [3.71],
[05], [133], [2.98], [3.04],
[10], [93], [2.08], [2.13],
[07], [81], [1.82], [1.85],
[09], [39], [0.87], [0.89],
[11], [32], [0.72], [0.73],
[14], [13], [0.29], [0.30],
[12], [11], [0.25], [0.25],
[13], [9], [0.20], [0.21],
[20 und höher], [5], [0.11], [0.11],
[15], [2], [0.04], [0.05],
[17], [2], [0.04], [0.05],
[16], [1], [0.02], [0.02],
[18], [1], [0.02], [0.02],
[19], [0], [0.00], [0.00],
[NAs], [92], [2.06], [NA],
[Total], [4461], [100.00], [100.00],

    // tinytable footer after

  ) // end table

  // tinytable align-figure after

] // end block
) // end figure
```<img src="figure/sub_chunk_2-1.svg" alt="" width="100%" style="display: block; margin: auto;" />
 
