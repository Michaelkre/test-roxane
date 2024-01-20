## Musterloesung Blatt 01

library(testthat)
library(microbenchmark)

## Aufgabe 1

# ggt - Berechnet den groessten gemeinsamen Teiler 
# mit Hilfe des Euklidischen Algorithmuses.
# Negative Zahlen und b > a sind auch moeglich!
#
# Eingabe:
#   a - Ganze Zahl. 
#   b - Ganze Zahl.
# Ausgabe
#   Groesster gemeinsamer Teiler von a und b (ganze Zahl).
ggt <- function(a, b) {
  while(b != 0) {
    tmp <- a %% b
    a <- b
    b <- tmp
  }
  return(abs(a))
}

testGGT <- function() {
  test_that("simple cases", {
    expect_equal(ggt(0L, 1L), 1L)
    expect_equal(ggt(1L, 2L), 1L)
    expect_equal(ggt(26L, 34L), 2L)
    expect_equal(ggt(58L, 145L), 29L)
    expect_equal(ggt(1000001L, 1048576L), 1L)
  })
  
  # Aufeinanderfolgende Fibonacci. Zahlen haben jeweils ggt 1. Es gilt
  # ggt(F_n, F_(n+1)) == F_{ggt(n, n+1)} und ggt(n, n+1) == 1
  # Ausserdem ist die Anzahl der Divisionen maximal in diesem Fall.
  test_that("fibonacci numbers", {
    expect_equal(ggt(55L, 34L), 1L)
    expect_equal(ggt(-55L, 34L), 1L)
    expect_equal(ggt(55L, -34L), 1L)
    expect_equal(ggt(-55L, -34L), 1L)
  })
}

testGGT()

# Testbeispiele wurden gewaehlt nach den Kriterien:
# 1) Die Wahrheit muss bekannt oder ueberpruefbar sein, ansonsten
# 2) Die Testfaelle sollten moeglichst unterschiedlich sein und moeglichst
#    alle Ablaeufe in der Funktion abtesten. Jedes Gebiet des Eingaberaums,
#    das ein anderen Verhalten haben koennte, sollte getestet werden
# 3) Vor allem Randfaelle sind interessant
# 4) Nicht zu vergessen: Die Tests sollten schnell sein


## Aufgabe 2
# Siehe PDF


## Aufgabe 3

## power1 - Setzt rekursive Multiplikation um, um n-te Potenzen zu berechnen.
## Eingabe:   x - Basis, reelle Zahl
##            n - Exponent, natuerliche Zahl
## Ausgabe:   Ergebnis x^n, numerischer Wert

power1 <- function(x, n){
  base <- x
  repeat{
    if(n == 1)
      return(x)
    x <- x * base
    n <- n - 1
  }
}

# Hilfsfunktion: Ist eine eingegebene Zahl gerade?

isOdd <- function(x) (x %% 2) == 1

testIsOdd <- function() {
  expect_equal(isOdd(2), FALSE)
  expect_equal(isOdd(3), TRUE)
}
testIsOdd()

## power2 - Setzt bessere rekursive Multiplikation um, um n-te Potenzen zu berechnen.
## Eingabe:   x - Basis, reelle Zahl
##            n - Exponent, natuerliche Zahl
## Ausgabe:   Liste:  power - numerisches Ergebnis x^n
##                    mults - Anzahl benoetigter Multiplikationen

power2 <- function(x, n) {
  res <- 1
  mults <- 0
  while(n != 0) {
    if(isOdd(n)) {
      res <- res * x
      mults <- mults + 1
      n <- n - 1
    } else {
      x <- x * x
      mults <- mults + 1
      n <- n / 2
    }
  }
  return(list(power = res, mults = mults))
}


power3 <- function(x, n){
  return(x^n)
}

## Optionen in microbenchmark: Setze times auf 1000 statt default 100, um robustere
## Schaetzungen zu erhalten. unit = "ns" fuer Nanosekunden, fuer Vergleichbarkeit.

mean(microbenchmark(power1(1.5, 200), unit = "ns", times = 1000)$time)
# 10482
mean(microbenchmark(power2(1.5, 200), unit = "ns", times = 1000)$time)
# 6257
mean(microbenchmark(power3(1.5, 200), unit = "ns", times = 1000)$time)
# 483

## Wenig ueberraschend ist die voll-rekursive Methode sehr ineffizient. Unsere
## Aenderungen in power2 bringen zwar Verbesserung, kommen aber nicht annaehernd
## an die Basis-Implementierung in R heran.
## Unter anderem das Zaehlen der Multiplikationen benoetigt natuerlich auch Zeit.

## Fuer eine Grafik:

res1 <- sapply(1:200, function(n) mean(microbenchmark(power1(1.5, n), unit = "ns", times = 1000)$time))
res2 <- sapply(1:200, function(n) mean(microbenchmark(power2(1.5, n), unit = "ns", times = 1000)$time))
res3 <- sapply(1:200, function(n) mean(microbenchmark(power3(1.5, n), unit = "ns", times = 1000)$time))

plot(1:200, res1, type = "l", ylab = "Laufzeit in ns", xlab = "Exponent n")
points(1:200, res2, type = "l", col = "blue")
points(1:200, res3, type = "l", col = "green")
legend(x="topleft", lty = 1, col = c("black", "blue", "green"), legend = c("power1", "power2", "power3"))

rm(res1, res2, res3)

## Gut zu sehen: Die Laufzeit von power1 steigt linear in n. Die Laufzeit von 
## power2 steigt global nicht linear, hat aber starke Schwankungen. 
## Vergleiche dazu x^7 und x^8: 
## x^8 benoetigt nur 4 Multiplikationen, x^7 benoetigt 5 Multiplikationen.

