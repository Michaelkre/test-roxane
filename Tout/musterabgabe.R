## Diese Datei stellt eine Musterabgabe fuer eine Pseudo-Aufgabe dar. In dieser
## Datei findet ihr saemtliche Kommentare, Tests, Checks, u.s.w., die wir von
## euch erwarten. In dieser Datei findet ihr 2 Arten von Kommentaren. Die mit
## lediglich einem # angekuendigten Kommentare sind Kommentare, die auch in 
## einer von euren Abgaben auftauchen muessten. Bei 2 ##, wie in diesem Absatz,
## erfolgen Erlaeuterungen zu dieser Datei.

# ************************************************
# *          CStat2022 Musterabgabe              *
# *          Abgabe von Daniel Horn              *
# *             Freitag, 10-12 Uhr               *
# ************************************************

## Zunaechst mit library() und source() alle zusaetzlichen Dateien und
## Bibliotheken laden, die benoetigt werden. Dabei bei den Kommentaren
## aufpassen, dass nicht mehr als 80 Zeichen in einer Zeile stehen, damit das
## beim Ausdrucken keine haesslichen Zeilenumbrueche gibt.

library(testthat)
## source(zusaetzlicheDateienFuerDieAufgaben) - wird hier nicht benoetigt,
## kann aber auf zukuenftigen Zetteln benoetigt werden. Achtet darauf, an
## dieser Stelle relative Pfade zu benutzen! Ihr koennt davon ausgehen, dass
## alle weiteren Dateien bei uns im gleichen Ordner wie eure Abgabe liegen.
## Jetzt kommt die Bearbeitung der einzelnen Aufgaben, zu Anfang je durch einen
## geeigneten Kommentar angekuendigt.

# Aufgabe 1

## In dieser Aufgabe wird jetzt beispielhaft eine Funktion implementiert, die
## entweder die Summe oder das Produkt eines Vektor mit einem Skalar bestimmt.
## Dazu wird zunaechst der Funktionskopf dokumentiert - diese paar Zeilen 
## Dokumentation sind verpflichtend fuer (fast) jede Funktion, die ihr schreibt.
## Ausnahmen koennen 2-3-zeilige, selbsterklaerende Hilfsfunktionen sein.

# myArithSwitcher - berechnet entweder Summe oder Produkt eines Vektors mit 
#                   einem Skalar
#
# Input: x    - ein numerischer Vektor
#        a    - ein numerischer Skalar
#        type - entweder "sum", "prod" oder "both". Gibt an, ob die Summe x + a,
#               das Produkt x * a oder beides bestimmt werden soll
#
# Output:
#  Eine benannte Liste. Je nach Wert von type enthaelt die Liste die Elemente
#        sum - die Summe von x und a
#        prod - das Produkt von x und a

## Da Funktionen in R nur einen einzigen Rueckgabewert haben, oftmals aber
## mehrere Objekte zurueckgegeben werden sollen, ist der Rueckgabewert einer
## Funktion fast immer eine benannte Liste.

myAritSwitcher <- function(x, a, type) {
  ## Zunaechst muss ueberprueft werden, ob die 3 eingegebenen Parameter vom
  ## richtigen Typ sind. Der Benutzer der Funktion koennte fuer x ja auch einen
  ## character-Vektor eingeben, in dem Fall wird ein Fehler zurueckgegeben.
  stopifnot(is.numeric(x),
            is.numeric(a),
            length(a) == 1L,
            is.vector(x),
            type %in% c("sum", "prod", "both"))
  ## Wer mag, kann sich einmal das Paket 'checkmate' anschauen. Dies ist dafuer
  ## da die ParamChecks etwas eleganter aufzuschreiben.
  
  result <- list()
  if(type != "prod")
    result$sum <- x + a
  if(type != "sum")
    result$prod <- x * a
  
  return(result)
}

## Jetzt muss noch getestet werden, ob die obige Funktion tatsaechlich macht,
## was sie machen soll. Dies ist essentiell wichtig! Vor allem, wenn ihr
## irgendwann einmal selbst Software in einer Gruppe von Leuten entwickelt,
## muss es fuer eure Funktionen automatisierte Tests geben. Damit es bei
## Aenderungen am grossen Ganzen des Codes auffaellt, wenn alte Funktionalitaet
## zerstoert wird. Zum Testen bietet sich das Paket 'testthat' an, das bereits
## oben geladen wurde.
## Das Testen einer Funktion ist an sich immer notwendig. Wir verlangen es aber
## nur dann von euch, wenn es explizit in der Aufgabenstellung steht.


testMyArithSwitcher <- function() {
  test_that("please find a good name", {
    res1 <- myAritSwitcher(1:5, 2, "sum")
    res2 <- myAritSwitcher(1:5, 2, "prod")
    res3 <- myAritSwitcher(1:5, 2, "both")
    expect_equal(typeof(res1), "list")
    expect_equal(length(res1), 1)
    expect_equal(res1$sum, 3:7)
    
    expect_equal(typeof(res2), "list")
    expect_equal(length(res2), 1)
    expect_equal(res2$prod, seq(2, 10, by = 2))
    
    expect_equal(typeof(res3), "list")
    expect_equal(length(res3), 2)
    expect_equal(res3$sum, 3:7)
    expect_equal(res3$prod, seq(2, 10, by = 2))
  })
}

## Und zum Abschluss, ganz wichtig, den Test einmal Ausfuehren
testMyArithSwitcher()