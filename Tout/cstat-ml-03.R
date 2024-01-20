source("turingsimulator.R")

## Aufgabe 2

# Generelle Idee der Maschine:
# Suche von Links nach Rechts. Wenn eine 1 gefunden wurde, ersetze diese durch
# eine 0. Gehe weiter nach rechts und suche eine 0, ersetze diese durch eine 1.
# Dadurch wurden die beiden Elemente "Getauscht".
# Gehe jetzt zurueck zum Bandanfang und wiederhole, bis das Band sortiert ist.

prog.sort <- list(
  # Zustand 0: Startzustand. Der Zeiger ist auf jeden Fall auf einem b.
  # Gehe nach rechts und starte die Maschine
  list(0, "b", "b", "R", 1),
  # Zustand 1: Gehe nach rechts bis eine 1 gefunden wird, ersetze diese durch 0
  # Falls keine 1 gefunden wird, besteht das Band nur aus 0en und ist sortiert
  list(1, "b", "b", "L", 5),
  list(1, "0", "0", "R", 1),
  list(1, "1", "0", "R", 2),
  # Zustand 2: Jetzt muss die naechste 0 gefunden und durch 1 ersetzt werden.
  # Falls keine 0 gefunden wird, ist das Band sortiert. In diesem Fall haben
  # wir zuvor faelschlicherweise 1 durch 0 ersetzt, dies muessen wir
  # rueckgaengig machen
  list(2, "b", "b", "L", 4),
  list(2, "0", "1", "L", 3),
  list(2, "1", "1", "R", 2),
  # Zustand 3: Gehe zurueck zum Bandanfang und wechsle nach Zustand 1
  list(3, "b", "b", "R", 1),
  list(3, "0", "0", "L", 3),
  list(3, "1", "1", "L", 3),
  # Zustand 4: Wir haben in Zustand 2 keine 0 gefunden, daher muss die
  # geloeschte 1 aus Zustand 1 wieder zur 1 werden.
  list(4, "0", "1", "L", 5),
  list(4, "1", "1", "L", 4),
  # Zustand 5: Wir haben fertig sortiert. Kehre zurueck zum Bandanfang
  # und beende das Programm
  list(5, "b", "b", "N", 6),
  list(5, "0", "0", "L", 5),
  list(5, "1", "1", "L", 5)
)

# Test

testMyTM = function() {
  # Funktion um einen Durchlauf fuer ein gegebenes Band zu testen
  singleTest = function(ini.tape) {
    # TM laufen lassen
    res = runTuringMachine(prog.sort, tape = ini.tape,
      L0 = 0L, F = 6L, ini.pos = 1L)
    # Pruefen ob Ergebnis sortiert ist. Spezialfall leere Eingabe beachten.
    if (length(res) > 2L)
      !is.unsorted(as.numeric(res[2:(length(res)-1)]))
    else
      all(res == c("b", "b"))
  }
  ## Spezialfaelle
  stopifnot(
    singleTest(c("b", "b")),
    singleTest(c("b", "1", "b")),
    singleTest(c("b", "0", "b")),
    singleTest(c("b", "0", "1", "b")),
    singleTest(c("b", "1", "1", "1", "b")),
    singleTest(c("b", "0", "0", "0", "b"))
  )
  # 20 Tests mit zufaelligem "langem" Band
  set.seed(1273)
  tapes = replicate(20, c("b", sample(c(0, 1), 20, replace = TRUE), "b"))
  stopifnot(all(apply(tapes, 2, singleTest)))
}

testMyTM()
