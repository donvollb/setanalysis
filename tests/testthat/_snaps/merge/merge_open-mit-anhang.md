### 1.34 [FILTER_123] Welche weiteren Informationen hätten Sie gerne gehabt, damit Sie sich vor Beginn Ihres Studiums ausreichend über den Studiengang informiert gefühlt hätten? {#sec-1.top} 

*Die offenen Antworten zu dieser Frage finden sich* [im Anhang](#sec-1.bottom).  

\

### 1.35 Leere Frage {#sec-2.top} 

*Keine offenen Antworten zu dieser Frage.*  

\

# Anhang: Fragen mit offenem Antwortformat  
  
### 1.34 [FILTER_123] Welche weiteren Informationen hätten Sie gerne gehabt, damit Sie sich vor Beginn Ihres Studiums ausreichend über den Studiengang informiert gefühlt hätten? {#sec-1.bottom} 
 
[zurück nach oben](#sec-1.top) 

*Die folgenden Antworten wurden jeweils nur einmal gegeben:*  


```{=typst}
#show figure: set block(breakable: false)
#figure( // start preamble figure
  
  kind: table, // end preamble figure

block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "1_0": 0, "2_0": 0, "3_0": 0, "4_0": 0, "5_0": 0, "6_0": 0, "7_0": 0, "8_0": 0, "9_0": 0, "10_0": 0, "11_0": 0, "12_0": 0, "13_0": 0, "14_0": 0, "15_0": 0, "16_0": 0, "17_0": 0, "18_0": 0, "19_0": 0, "20_0": 0, "21_0": 0, "22_0": 0, "23_0": 0, "24_0": 0, "25_0": 0, "26_0": 0, "0_0": 1
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: left,),
    (bold: true, align: left,),
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
    columns: (100.00%),
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
 table.hline(y: 1, start: 0, end: 1, stroke: 0.05em),
 table.hline(y: 27, start: 0, end: 1, stroke: 0.08em),
 table.hline(y: 0, start: 0, end: 1, stroke: 0.08em),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[Antwort],
    ),
    // tinytable header end

    // tinytable cell content after
['\-],
['\- Keine Infos über Prozedere der Modulwahl (Module können nicht frei gewählt werden, sondern werden nach Priorisierung zugeteilt) \- Genauere Infos über Module erfolgte erst nach Modulwahl, danach ist jedoch kaum noch eine Änderung möglich],
['\-Welche Vorkenntnisse und wissen benötigt wird.  \-],
[Anerkennung Vorstudien Leistungen, Wahlpflichtleistungen,],
[Anstelle der ganzen QR\-Codes in der Infobroschüre mehr Fließtext zum Lesen.],
[Dass es überhaupt einen 2 F B gibt und was das ist],
[Die Studienverlaufspläne waren nicht gut zu finden. Der Studiengang wird von zwei Fakultäten parallel organisiert, was sich negativ auf die Inhalte auswirkt. Der Wert des Abschlusses war so nicht ersichtlich.],
[Einzelne Vorlesungen],
[Exakte Prüfungsformen],
[Genauere Informationen zum Ablauf des ersten Semesters],
[Ich fand das Informationen für die erste Woche etwas spät kam],
[Ich hätte mir mehr Details zum Ablauf des Studium gewünscht],
[Inhalte, Anforderungen],
[Keine angabe],
[Klips Hilfe],
[Man muss erstmal wissen, was wichtig zu wissen ist. Vieles zieht an einem vorbei. Wenn man frisch an eine Uni kommt, hat man ja null Plan.],
[Mehr Informationen am Infotag von der Fachschaft],
[Mehr Informationen durch Studierende\/aus der Perspektive von Studierenden.],
[Mögliche Vorlesungen, ERKLÄRUNG DES STUNDENPLANS (WÄHLEN)],
[Nachvollziehbare Übersicht über die Inhalte des Studiums.],
[Studienverlaufsplan in Philosophie],
[Videos von Dozenten und Studierenden, die über den Studiengang berichten, um einen realistischen Eindruck zu erhalten anseits von Modulhandbüchern etc.],
[Zugang zu Studienverlaufspläne erläutern],
[alle],
[genauere Angabe über Berufsfelder und Anforderungen in den Naturwissenschaften (v.a. in Chemie und Biologie)],
[Übersichtlicheres Modulhandbuch],

    // tinytable footer after

  ) // end table

  // tinytable align-figure after

] // end block
) // end figure
``` 

### 1.35 Leere Frage {#sec-2.bottom} 
 
[zurück nach oben](#sec-2.top) 

*Keine offenen Antworten zu dieser Frage.*  

