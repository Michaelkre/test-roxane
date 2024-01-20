## Aufgabe 1

## a)
# Sampeln auf dem Einheitskreis
mySampler <- function(n, d) {
  mat <- matrix(runif(n * d, -1, 1), ncol = d)
  t(apply(mat, 1, function(x) x / sqrt(sum(x^2))))
}
plot(mySampler(1000, 2))

## b)
# matCumSum - bestimmt die zeilenweise oder spaltenweise kumulierte Summe
# Input: mat - numerische Matrix
#        margin - (numerischer Skalar) Wie soll summiert werden?
#                     1 enspricht zeilenweise, 2 entspricht spaltenweise
# Output: Numerische Matrix derselben Dimension wie mat, die spaltenweise die  
#           zeilenweisen/ spaltenweisen kumulierten Summen enthaelt
#
matCumSum <- function(mat, margin) {
  res <- apply(mat, margin, cumsum)
  return(res)
}

matrix(1:25, 5, 5)
matCumSum(matrix(1:25, 5, 5), 1)
matCumSum(matrix(1:25, 5, 5), 2)


## Aufgabe 2

# Hier das Beispiel, Erklaerung im PDF

g <- function(x, a) {
  sum(x^(2:0) * a)
}

g(1000000, c(0.1, 0.1, -100000100000.000001))



## Aufgabe 3

## a) s. PDF

## b)
##
## Es gibt natuerlich viele Moeglichkeiten die naive sum-Funktion smart zu
## machen. Ein paar sind hier aufgefuehrt. 
## Zunaechst zum Vergleich der naive Ansatz, auch mit der bitte hier einmal
## auf den Progammierstil, vor allem auf die For-Schleife zu achten.


## naiveSum - summiert einen Vektor von Zahlen
##
## Eingabe:
##  v - Vektor von reellen Zahlen
##
## Ausgabe:
##  Summe der Zahlen in 'v'.
naiveSum <- function(v) {
  stopifnot(is.numeric(v), all(is.finite(v)))
  s <- 0
  for(num in v) {
    s <- s + num
  }
  s
}


## smart_sum - summiert einen Vektor von Zahlen
##
## Eingabe:
##  v - Vektor von reellen Zahlen
##
## Ausgabe:
##  Summe der Zahlen in 'v'.
##
## Der Algorithmus geht auf William Kahan zurueck. Siehe:
##
##   Kahan, William (1965), "Further remarks on reducing truncation
##   errors", Communications of the ACM 8 (1): 40

smartSum <- function(v) {
  stopifnot(is.numeric(v), all(is.finite(v)))
  sum <- 0
  err <- 0
  for(num in v) {
    tmp <- sum
    y <- num + err
    sum <- tmp + y
    err <- (tmp - sum) + y
  }
  sum + err
}


## smart2_sum - summiert einen Vektor von Zahlen
##
## Eingabe:
##  v - Vektor von reellen Zahlen
##
## Ausgabe:
##  Summe der Zahlen in 'v'.
##
## Der Algorithmus benutzt einen Rekursionsansatz, um moeglichst
## gleich grosse Zahlen zu addieren.

smartSum2 <- function(v) {
  stopifnot(is.numeric(v), all(is.finite(v)))
  n <- length(v)  
  if (n == 1L) {
    v
  } else if (n == 2L) {
    v[1] + v[2]
  } else {
    m <- n %/% 2 + 1
    Recall(v[1:m]) + Recall(v[(m+1):n])
  }
}


## smart_sum3 - summiert einen Vektor von Zahlen
##
## Eingabe:
##  v - Vektor von reellen Zahlen
##
## Ausgabe:
##  Summe der Zahlen in 'v'.
##
## Der Algorithmus benutzt eine Min Heuristik, um betragsmaessig moeglichst
## gleich grosse Zahlen zu addieren.

smartSum3 <- function(v) {
  stopifnot(is.numeric(v), all(is.finite(v)))
  while (length(v) > 2L) {
    ## FIXME: Ineffizient!
    l <- which.min(abs(v))
    vl <- v[l]
    v <- v[-l]
    u <- which.min(abs(v))
    vu <- v[u]
    v <- c(v[-u], vl + vu)
  }
  if (length(v) == 2L)
    v[1] + v[2]
  else
    v
}

## smart_sum4 - summiert einen Vektor von Zahlen
##
## Eingabe:
##  v - Vektor von reellen Zahlen
##
## Ausgabe:
##  Summe der Zahlen in 'v'.
##
## Der Algorithmus sortiert die Zahlen zunaechst nach ihren Betraegen,
## um zunaechst grosse und dann erst kleine Zahlen zu bearbeiten.

smartSum4 <- function(v) {
  stopifnot(is.numeric(v), all(is.finite(v)))
  order <- order(abs(v), decreasing = TRUE)
  naiveSum(v[order])
}


compareSums <- function(v, true) {
  bs <- sum(v)
  ns <- naiveSum(v)
  ks <- smartSum(v)
  rs <- smartSum2(v)
  ms <- smartSum3(v)
  as <- smartSum4(v)
  
  df <- data.frame(method = c("baseR", "naive", "kahan", "recursive", "minmin", "abs"),
                   value = c(bs, ns, ks, rs, ms, as),
                   delta = abs(true - c(bs, ns, ks, rs, ms, as)))
  print(df)
}

doItA3 <- function(const) {
  set.seed(1273)
  x <- runif(50, 0, 1e10)
  x <- sample(c(-x, x, const))
  compareSums(x, const)
}


doItA3(5.3)
doItA3(0.1)

## smartSum4 gewinnt - hat aber den grossen Nachteil einer n log n Laufzeit
## (es muss sortiert werden). Und wir sehen: Wie ueblich ist die base R
## Funktion schon ziemlich intelligent.
## In der Praxis also bitte sum benutzen - wir sind nicht schlauer genug
## diese in 2h besser neu zu schreiben.


