load("schokolade.RData")

set.seed(1234)

## Aufgabe 1

## a)

# Wir muessen von der Summe der u_i lediglich ihren Erwartungswert
# abziehen und durch ihre Standardabweichung dividieren ("standardisieren")
# Dann haben wir nach dem zentralem Grenzwertsatz approximativ normalverteilte
# Zufallszahlen.
#
# Der Erwartungswert eines u_i ist 0.5, daher ist der Erwartungswert von
# der Summe k * 0.5. Darum muss im Skript bei k = 12 auch 6 abgezogen werden
#
# Die Varianz eines u_i ist gerade 1/12. Die Varianz der Summe ist die Summe
# der Varianzen, als k / 12. Und noch die Wurzel wegen Standardabweichung.
# Im Skript mit k = 12 ergibt sich also gerade der Faktor 1 und faellt daher
# weg

## b)
# myRNorm1 - Simulation von normalverteilten Zufallszahlen
#
# Input
#   n (Integer) - Anzahl der zu erzeugenden Zufallszahlen
#   k (Integer) - Anzahl der summierten gleichverteilten Zufallzahlen
# Output
#  Numerischer Vektor mit n simulierten Zufallzahlen
myRNorm1 <- function(n, k) {
  (rowSums(matrix(runif(n * k), nrow = n)) - k / 2 ) * sqrt(12 / k)
}

## c)
# qualitiy erzeugt eine Graphik, die die Qualitaet von b) ueberprueft
# ks ist ein Vektor von ganzen Zahlen und gibt die zu verwendenden Werte
# fuer k an. Ausgabe ist ein boxplot
qualitiy <- function(ks) {
  res <- sapply(ks, function(k)
    replicate(1000, shapiro.test(myRNorm1(1000, k = k))$p.value))
  boxplot(res, main = "Quality of Normal RNG", ylab = "p.values", xlab = "k")
  abline(h = c(0.25, 0.5, 0.75), lwd = 2, col = "red")
  op <- par(mfrow = c(5, 5), mar = c(4, 5, 2, 1), oma = c(0, 0, 1, 0), 
            mgp = c(2, 0.7, 0))
  sapply(seq(along = ks), function(i) {
    hist(res[, i], freq = FALSE, xlab = "p.values", main = paste0("k = ", ks[i]), 
         xlim = 0:1)
    abline(h = 1, col = "red", lwd = 2)
  })
  title(main = "Quality of Normal RNG", outer = TRUE)
  par(op)
}
qualitiy(1:25)

# Schwierige Frage, alles unter 6 bis 8 ist Mist. Danach geht es langsam aufwaerts,
# mehr ist natuerlich immer besser, aber ab cirka 15 scheint es nur noch
# sehr langsam besser zu werden. Mit der 12 aus dem Skript kann man schon leben
# ein paar waeren schon, mehr als 25 braucht es aber sicherlich nicht.



## Aufgabe 2

# mRbeta - Funktion, welche mittels Rejection Sampling (Verwerfungsmethode)
#         beta-verteilte Zufallszahlen zieht
# Eingabe:
#     n - Anzahl der Zufallszahlen, natuerliche Zahl
#     p - Parameter der Beta-Verteilung, reelle Zahl (nach Hinweis p >= 1)
#     q - Parameter der Beta-Verteilung, reelle Zahl (nach Hinweis q >= 1)
# Ausgabe:
#     Vektor von n beta-verteilten Zufallszahlen

mRbeta <- function(n, p, q) {

  # Beachte den Unterschied zwischen 'q' als Parameter der Beta-Verteilung und
  # 'q()' als Dichte (der Gleichverteilung) wie in der Vorlesung!

  # Zunaechst muessen wir das k bestimmen, den oberen Rand der Einhuellenden,
  # siehe Folien 304:
  # Zum Glueck wissen wir, dass q() die Rechteckverteilung und somit
  # konstant 1 ist. Damit faellt der Nenner direkt weg und wir brauchen
  # nur noch das Maximum der Dichtefunktion der Beta-Verteilung mit den
  # jeweiligen Parametern.
  # Dazu berechnen wir den Modalwert (im Intervall von 0 bis 1) und werten die
  # Dichte der Beta-Verteilung an dieser Stelle aus (s. z.B. Wikipedia).
  
  if (any(p < 1, q < 1)) {
    # Im Spezialfall, dass p oder q kleiner 1 ist, trunkieren wir die
    # Verteilung am Rand (exakt 0 oder 1 werden durch runif() "fast nie"
    # erreicht), d.h. wir verwenden den hoechsten Wert der Dichte, der nahe 0
    # oder nahe 1 erreicht wird (auf diesem Rand geht die Dichte gegen
    # unendlich):
    k <- max(dbeta(c(1e-3, 1 - 1e-3), p, q))
  }
  else {
    if (any(p == 1, q == 1)) {
      # Sollte p oder q genau 1 sein (und der andere Parameter mindestens 1),
      # liegt der Modalwert wie zuvor auf dem Rand, die Dichte ist hier aber
      # endlich und es gilt:
      k <- max(p, q)
    }
    else {
      # im Uebrigen (p und q sind echt groesser 1) liegt der Modalwert im
      # Inneren des Intervalls und es gilt:
      modus <- (p - 1) / (p + q - 2)
      k <- dbeta(modus, p, q)
      # (Diese Definition schliesst eigentlich auch den Fall ein, dass p oder q
      # genau 1 ist und der andere Parameter echt groesser 1, da dann der
      # Modalwert 0 bzw. 1 wird und die Dichte dort endlich ist.
      # Wegen des hier problematischen Falles p = q = 1 bleiben wir zur
      # Vereinfachung und Uebersichtlichkeit bei dieser Unterscheidung.)
    }
  }
  
  # Mit diesem k gehen wir wie auf Folie 305 bzw. 307 vor:
  # wiederholen, bis n erreicht ist:
  res <- replicate(n, {
    # jeweils wiederholen, bis eine gezogene Zufallszahl akzeptiert wird:
    repeat {
      # ziehen von x aus q(), der Gleichverteilung:
      x <- runif(1, 0, 1)
      # ziehen von u:
      u <- runif(1, 0, 1)
      # der Quotient, mit dem u verglichen werden soll (vereinfacht durch
      # q() = 1), eine Wahrscheinlichkeit:
      prob <- dbeta(x, p, q) / k
      # Dieser Wert ist in dem Spezialfall, dass p oder q kleiner als 1 ist
      # und x nah an den Raendern des Intervalls von 0 bis 1 gezogen wurde,
      # nicht verwendbar, da k hier nicht passend gewaehlt wurde.
      # Dies ist daran erkennbar, dass die Wahrscheinlichkeit groesser als 1
      # wird und der Schritt wird uebersprungen:
      if(prob > 1) {
        next
      }
      ## x ggf. akzeptieren:
      if(u <= prob) {
        break
      }
    }
    x
  })
  res
}


check <- function(p, q) {
  hist(mRbeta(10000, p, q), freq = FALSE)
  # Das Histogramm der gezogenen Zufallszahlen wird mit der Dichte der Beta-
  # Verteilung verglichen:
  curve(dbeta(x, p, q), from = 1e-3, to = 1-1e-3, lwd = 2, col = "red", 
        add = TRUE)
}
check(1, 1)
check(1, 2)
check(1, 5)
check(5, 1)
check(2, 3)
check(3.4, 2.7)
# Hier sieht man, dass das Verfahren trotz der trunkierten Werte relativ
# vernuenftig funktioniert:
check(0.5, 0.5)
# Man sieht aber auch, dass es ab hier relativ lange Laufzeiten gibt:
check(0.2, 1)


# Zur Anzahl der benoetigten Ziehungen:

# Sei z nun die aus q() gezogene Zufallszahl und seien p und q mindestens 1.

# Der Wert prob = dbeta(z, p, q) / k aus der obigen Funktion stellt sich bei
# auf [0,1] gleichverteiltem u als Akzeptanzwahrscheinlichkeit fuer z heraus.
# Aus dieser laesst sich dann die gesuchte Anzahl bestimmen.

# Im Falle p = q = 1 sind k und dbeta(z, p, q) immer 1 und damit auch prob = 1.
# Damit werden die z sicher akzeptiert und es ist fuer alle z aus [0, 1] nur
# eine Ziehung noetig, um eine beta-verteilte Zufallszahl zu erzeugen.

# Wir beschraenken uns deshalb auf den Fall, dass p oder q echt groesser 1 ist:

# Wie oben unterschieden gilt dann modus = (p - 1) / (p + q - 2) und
# k = dbeta(modus, p, q).
# Damit ist prob der Quotient aus zwei beta-Dichten an den Stellen z und modus.
# Nach Kuerzen der Beta-Funktion vereinfacht sich dieser zu
prob <- function(z, p, q) {
  (z / ((p - 1) / (p + q - 2)))^(p - 1) * 
    ((1 - z) / ((q - 1) / (p + q - 2)))^(q - 1)
}
# (wenn 0^0 als 1 betrachtet wird).

curve(prob(x, 1, 2), xlab = "z", ylab = "prob", main = "p = 1, q = 2")
curve(prob(x, 2, 2), xlab = "z", ylab = "prob", main = "p = 2, q = 2")
curve(prob(x, 5, 2), xlab = "z", ylab = "prob", main = "p = 5, q = 2")

# Die mittlere Anzahl der benoetigten Ziehungen bis zu einer Akzeptanz (d.h.
# zur Erzeugung einer beta-verteilten Zufallszahl) ist der Kehrwert von prob:
curve(1 / prob(x, 1, 2), xlab = "z", ylab = "Anzahl Ziehungen", 
      main = "p = 1, q = 2")
curve(1 / prob(x, 2, 2), xlab = "z", ylab = "Anzahl Ziehungen",
      main = "p = 2, q = 2")
curve(1 / prob(x, 5, 2), xlab = "z", ylab = "Anzahl Ziehungen",
      main = "p = 5, q = 2")
curve(1 / prob(x, 1, 2), xlab = "z", ylab = "Anzahl Ziehungen",
      main = "p = 1, q = 2", log = "y")
curve(1 / prob(x, 2, 2), xlab = "z", ylab = "Anzahl Ziehungen", 
      main = "p = 2, q = 2", log = "y")
curve(1 / prob(x, 5, 2), xlab = "z", ylab = "Anzahl Ziehungen",
      main = "p = 5, q = 2", log = "y")
# Diese Anzahl nimmt ggf. fuer kleines bzw. grosses z stark zu.

# Um die mittlere Anzahl noetiger Ziehungen unabhaengig vom realisierten z
# vorauszusagen, koennte man noch ueber z integrieren, was aber an den
# Raendern zu Schwierigkeiten fuehrt:
f <- function(z, p, q) {1 / prob(z, p, q)}
integrate(f, lower = 1e-9, upper = 1-1e-9, p = 1, q = 2)
# 20.72327 with absolute error < 6.3e-06
integrate(f, lower = 1e-9, upper = 1-1e-9, p = 2, q = 2)
# 10.36163 with absolute error < 0.00076
integrate(f, lower = 1e-3, upper = 1-1e-3, p = 5, q = 2)
# 27347710 with absolute error < 569
## (zu starke Divergenz)


## Aufgabe 3

## a)

## bootstrapTest - fuehrt einen Bootstrap-Test fuer mean(x) = mean(y) durch
#
# Eingabe:
#     x -     erster Stichprobenvektor, numerischer Vektor
#     y -     zweiter Stichprobenvektor, numerischer Vektor
#     n -     Anzahl der Bootstrap Stichproben, natuerliche Zahl
# Ausgabe:
#     numerischer Wert aus [0, 1], p-Wert

bootstrapTest <- function(x, y, n = 1000) {
  vert <- replicate(n, (mean(sample(x, replace = TRUE)) -
                          mean(sample(y, replace = TRUE))))
  p.l <- mean(vert > 0) 
  p.r <- mean(vert < 0)
  2 * min(p.l, p.r)
}

## b)

# bootstrapPower - schaetzt die Power des Tests durch
#                  Bootstrapping zufaelliger normalverteilter Stichproben
# Eingabe:
#     x      - erster Stichprobenvektor, numerischer Vektor
#     y      - zweiter Stichprobenvektor, numerischer Vektor
#     test   - Funktionsname des Tests
#     deltas - Unterschied zwischen Erwartungswerten von X und Y, numerisch.
#
# Ausgabe:
#   Geschaetzte Power des Tests (numerisch zwischen 0 und 1)

bootstrapPower <- function(x, y, test, deltas) {
  
  onePower <- function(delta) {
    mean(replicate(200, test(sample(x, replace = TRUE),
                             delta + sample(y, replace = TRUE)) < 0.05))
  }
  
  sapply(deltas, onePower)
}


makePowerComp <- function(left.data, right.data, deltas) {
  test1 <- function(x, y) t.test(x, y)$p.value
  test2 <- function(x, y) wilcox.test(x, y)$p.value
  test3 <- function(x, y) bootstrapTest(x, y, n = 200)
  
  plot(deltas, bootstrapPower(left.data, right.data, test1, deltas),
       type = "b", ylab = "power", xlab = "alternative", pch = 16)
  points(deltas, bootstrapPower(left.data, right.data, test2, deltas),
         type = "b", col = "red", pch = 16)
  points(deltas, bootstrapPower(left.data, right.data, test3, deltas),
         type = "b", col = "blue", pch = 16)
  abline(h = 0.05)
  legend("bottomleft", legend = c("t.test", "wilc.test", "boot.test"),
         col = c("black", "red", "blue"), lty = 1, pch = 16)
}

set.seed(1234)
x1 <- rbinom(100, 6, 0.25)
y1 <- rbinom(100, 6, 0.25)
y1 <- y1  - mean(y1) + mean(x1)
x2 <- rbinom(100, 6, 0.5)
y2 <- rbinom(100, 6, 0.5)
y2 <- y2  - mean(y2) + mean(x2)
x3 <- rbinom(100, 6, 0.25)
y3 <- rbinom(100, 6, 0.25)
y3 <- y3  - mean(y3) + mean(x3)

makePowerComp(x1, y1, seq(-1.25, 1.25, length.out = 25))

makePowerComp(x2, y2, seq(-1.25, 1.25, length.out = 25))

makePowerComp(x3, y3, seq(-1.25, 1.25, length.out = 25))

# Wilcoxon Test hat mehr Power, ist aber verschoben -> Testet nur auf Median
# Wilcoxon Test haelt das Niveau nicht ein (viele Bindungen zu erwarten), 
# Bootstrap Test ist z.T. etwas konservativ

# Bootstrap Test hat minimal mehr Power, t-Test ist weniger rechenintensiv
# Je nach Wichtigkeit von Rechenzeit sollte einer der beiden verwendet werden
# Wir verwenden den Bootstrap-Test wegen der etwas hoeheren Power und da die 
# Rechenzeit hier nicht zu hoch ist.

bootstrapTest(sorte1, sorte2)
# Wir koennen die Nullhypothese zum 5%-Niveau ablehnen. 

t.test(sorte1, sorte2)
# Liefert quasi dasselbe Ergebnis.

par(mfrow = c(2, 1))
barplot(proportions(table(sorte1)), main = "Sorte 1")
barplot(proportions(table(sorte2)), main = "Sorte 2")
par(mfrow = c(1,1))

# Sorte 2 scheint besser bewertet zu sein, der Nikolaus sollte also diese verteilen.