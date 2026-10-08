# inkl.-Logik: Soll eine Frage in den aktuellen Bericht? -------------------
#
# Mit input_tabelle() wird für jeden Bericht festgelegt, welche Fragen er
# enthält. Im Berichts-Template stehen diese Entscheidungen als Variablen wie
# `inkl.2.1` (Abschnitt 2, Frage 1) zur Verfügung, meist in der globalen
# Umgebung. Die merge-Funktionen fragen sie über ihre Argumente `inkl` und `nr`
# ab:
#
# - `inkl = TRUE` / `inkl = FALSE`: Die Frage wird (nicht) ausgegeben.
# - `inkl = "nr"` (Standard): Ohne Nummer (`nr = ""`) wird die Frage immer
#   ausgegeben. Mit Nummer entscheidet die Variable `inkl.<nr>`.

# Wert der Variable `inkl.<nr>` nachschlagen
#
# Der Name wird in `env` (der aufrufenden merge-Funktion) ausgewertet und von
# dort aus über die übergeordneten Umgebungen bis zur globalen Umgebung
# gesucht.
.inkl_value <- function(nr, env = parent.frame()) {
  eval(str2lang(paste0("inkl.", nr)), envir = env)
}

# Argument `inkl` auflösen
#
# `marker` ist der Wert von `inkl`, bei dem die Nummer ausgewertet wird
# (bei merge_subj() z. B. "nr1" bzw. "nr2").
.resolve_inkl <- function(inkl, nr, marker = "nr", env = parent.frame()) {
  if (inkl == marker) {
    inkl <- if (nr == "") TRUE else .inkl_value(nr, env)
  }
  inkl
}
