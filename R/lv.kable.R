#' Funktion zur Tabellenerstellung
#'
#' @param x Objekt (üblicherweise Dataframe)
#' @param caption Titel der Tabelle
#' @param col.width Vektor der Spaltenbreiten, bei "default" automatische Spaltenbreiten
#' @param bold Sollen die Kopfzeile fettgedruckt sein?
#' @param bold.col1 Soll die erste Zelle der Kopfzeile fettgedruckt sein?
#' @param digits Anzahl der Nachkommastellen
#' @param escape ?
#' @param striped Soll die Tabelle Streifen (Schattierungen) erhalten
#'
#' @returns Tabelle
#' @export
#'
#' @examples lv.kable(head(mtcars, 10))


lv.kable <- function(x, # Objekt (am besten dataframe)
                     caption = NULL, # caption der Tabelle
                     col.width = "default", # Spaltenbreite (Vektor, z.B. "c("30pt", "50pt")), bei "default" gibt es automatische Spaltenbreiten
                     bold = TRUE, # Soll der header fett sein?
                     bold.col1 = TRUE, # Soll die erste Zelle des Headers fett sein? Sinnvoll, falls alle bis auf erste Zelle fett sein sollen
                     digits = 2, # Wie viele Nachkommastellen in der Tabelle?
                     escape = FALSE, # escape Argument in kbl
                     striped = TRUE) # gestrifte Tabelle?
{
  #  if(ncol(x) != length(col.width) & col.width[1] != "default") {
  #    stop(paste0("Es gibt eine unterschiedliche Anzahl an definierten Spaltenbreiten (col.width), nämlich ",
  #                length(col.width), " und Spalten in der Tabelle, nämlich ", ncol(x), "."))
  #  }

  #  if (escape == FALSE) {
  #    if(bold == TRUE & bold.col1 == TRUE) {colnames(x) <- paste0("\\textbf{", colnames(x), "}")}
  #    if(bold == TRUE & bold.col1 == FALSE) {colnames(x)[-1] <- paste0("\\textbf{", colnames(x)[-1], "}")}
  #  }


  # Erstellen einer Flextable
  latex.table <-  flextable::flextable(x)

  # Überschreiben der Default-Einstellungen
  flextable::set_flextable_defaults(
    font.family = "Red Hat Text",
    hansi.family = "Red Hat Text",
    eastasia.family = "Red Hat Text",
    cs.family = "Red Hat Text",
    big.mark = "",
    na_str = "NA",
    font.size = 10
  )

  # Bisherige Beobachtung für Schriftart (Google Font, z.B. Red Hat Text) bei Flextable
  # 1. Schriftart muss lokal "Für alle Benutzer" installiert werden (damit Pfad C:\Windows\Fonts)
  # 2. Bei Red Hat Text sollten nur "Regular", "Italic", "Bold" installiert werden, sonst ist der fette Text in
  # Flextable nicht ganz Fett (nur "Medium" oder "SemiBold"?)
  # Monospace Fonts, die funktionieren: Courier New, Lucida Sans Typewriter
  #  gdtools::register_gfont(family = "Red Hat Text", subset = c("latin", "latin-ext")) # Google Font registrieren
  latex.table <-  flextable::font( # neben defaults nochmal aufrufen, sonst ist Legenden-Tabelle in Arial (?)
    latex.table,
    i = NULL,
    j = NULL,
    fontname = "Red Hat Text",
    part = "all",
  )

  # Schriftgröße anpassen
  latex.table <- flextable::fontsize( # neben defaults nochmal aufrufen, sonst ist Legenden-Tabelle in 11 pt (?)
    latex.table,
    i = NULL,
    j = NULL,
    size = 10,
    part = "all")

  # Striped Theme
  latex.table <- flextable::theme_zebra(
    latex.table,
    odd_header = "#ecf1f4", # RPTU blaugrau (10 %)
    odd_body = "transparent",
    even_header = "transparent",
    even_body = "#ecf1f4" # RPTU blaugrau (10 %)
  )

  if (striped == FALSE) {
    latex.table <- flextable::theme_zebra(
      latex.table,
      odd_header = "transparent", # RPTU blaugrau (10 %)
      odd_body = "transparent",
      even_header = "transparent",
      even_body = "transparent" # RPTU blaugrau (10 %)
    )
  }




  # Fallunterscheidung für Spaltenbreiten
  # keine Spaltenbreite angegeben: autofit
  # Spaltenbreite angegeben: fixed und width() verwenden
  if (col.width[1] == "default") {
    # Wenn keine Spaltenbreite angegeben: autofit
    latex.table <- flextable::set_table_properties(
      latex.table,
      layout = "autofit", # fixed (Spaltenbreite in width()-Funktion wird verwendet) oder autofit (Tabellenbreite width() innerhalb dieser Funktion wird verwendet; nicht für PDFs)
      width = 0, # kein Einfluss bei PDF; wird ignoriert wenn layout="fixed"
      align = NULL,
      opts_pdf = list(
        tabcolsep = 5 # Padding links und rechts für PDFs (latextable::padding nicht für PDF)
      )
    )
  }
  if (col.width[1] != "default"){
    # wenn Spaltenbreite festgelegt: fixed
    latex.table <- flextable::set_table_properties(
      latex.table,
      layout = "fixed", # fixed (Spaltenbreite in width()-Funktion wird verwendet) oder autofit (Tabellenbreite width() innerhalb dieser Funktion wird verwendet; nicht für PDFs)
      width = 0, # kein Einfluss bei PDF; wird ignoriert wenn layout="fixed"
      align = NULL,
      opts_pdf = list(
        tabcolsep = 5 # Padding links und rechts für PDFs (latextable::padding nicht für PDF)
      )
    )
    # Spaltenbreite anpassen
    latex.table <- flextable::width(
      latex.table,
      width = col.width, # von pt-Angaben in Quarto-Dokument grob umgerechnet in mm
      unit = "mm"
    ) # nur cm, mm, in
  }




  # Horizontale Rahmelinien
  latex.table <- flextable::hline_top(
    latex.table,
    border = officer::fp_border(color = "black", width = 1), #
    part = "header")
  latex.table <- flextable::hline_top(
    latex.table,
    border = officer::fp_border(color = "black", width = .5),
    part = "body")
  latex.table <- flextable::hline_bottom(
    latex.table,
    border = officer::fp_border(color = "black", width = 1),
    part = "body")

  #  if (col.width[1] != "default") {
  #    for (k in 1:length(col.width)) {
  #      latex.table <- column_spec(latex.table, k, width = col.width[k], latex_valign = "m")
  #    }  }



  #  if(bold == TRUE & escape == TRUE) {latex.table <- row_spec(latex.table, 0, bold=TRUE)}

  #  if(bold.col1 == FALSE) {latex.table <- column_spec(latex.table, 1, bold = FALSE)}


  latex.table

}
