## lcf - Implementierung der Linear Congruential Methode
##
## Eingabe:
##   n        - gewuenschte Anzahl an Zufallszahlen, natuerliche Zahl
##   m, a, ce - Parameter der LCF (Modulus, Multiplikator, Inkrement),
##              natuerliche Zahlen und m > seed, a, ce
##  seed      - Startwert, natuerliche Zahl
##
## Ausgabe:
##   Vektor der Pseudo-Zufallszahlen

lcf <- function(n, seed, a, ce, m) {
  res <- numeric(n)
  x <- seed
  for (i in 1:n) {
    ## einzelner Schritt der erzeugenden Rekursion
    x <- (a * x + ce) %% m
    ## speichere das Ergebnis
    res[i] <- x
  }
  return(res)
}