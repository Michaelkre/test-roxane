# textbookVar - Varianzberechnung nach der 'Textbuch Methode'
#
# @param x [numeric(n)]
#   Vektor
#
# @return [numeric] 
#   Varianz von x nach dem textbook Algorithmus
textbookVar <- function(x) {
  n = length(x)
  # Achtung: Kein "echtes" One-Pass, weil so in R tatsaechlich effizienter
  sum <- sum(x)
  sumSquares <- sum(x^2)
  (sumSquares - sum^2 / n) / (n - 1)
}

# twoPassVar - Varianzberechnung nach der 'Zwei Durchgaenge Methode'
#
# @param x [numeric(n)]
#   Vektor
#
# @return [numeric] 
#   Varianz von x nach dem two pass Algorithmus
twoPassVar <- function(x) {
  n <- length(x)
  sum((x - sum(x) / n)^2) / (n - 1)
}

# corTwoPassVar - Varianzberechnung nach der korrigierten two-pass Methode
# @param x [numeric(n)]
#   Vektor
#
# @return [numeric]
#   Varianz von x nach dem korrigierten two-pass.
corTwoPassVar <- function(x) {
  n <- length(x)
  meanx <- sum(x) / n
  l <- sum((x - meanx)^2)              # left side
  r <- (1 / n) * (sum(x - meanx))^2    # right side
  (l - r) / (n-1)
}

# youngsCramerVar - Varianzberechnung nach dem Updating Algorithmus von Youngs und
#                Cramer.
# @param x [numeric(n)]
#   Vektor
#
# @return [numeric]
#   Varianz nach dem Youngs-Cramer Updating Algorithmus
youngsCramerVar <- function(x) {
  stopifnot(is.vector(x), is.numeric(x), all(is.finite(x)))
  n <- length(x)
  if (n <= 1) {
    0L
  } else {
    TT <- x[1]
    S <- 0L
    for (i in 2:n) {
      TT <- TT + x[i]
      S <- S + 1L / (i * (i - 1L)) * (i * x[i] - TT)^2
    }
    S / (n - 1L)
  }
}

## Helferfunktion fuer den pairwise Varianz Algorithmus. Rekursiver Algorithmus,
## der die eigentliche Arbeit macht. Um den komplizierten Output (Liste mit
## den beiden Elementen T und S) zu verstecken, wird der untere Wrapper
## genutzt.
