## Aufgabe 2

## a)

#  twoPass - Varianzberechnung nach der 'two pass Methode'
#
#  Input
#   x - numerischer Vektor
#
# Output
#   Varianz von x, numerischer Skalar
twoPass <- function(x) {
  return(sum((x - mean(x))^2) / (length(x) - 1))
}

## b)
#  textBook - Varianzberechnung nach der 'textBook Methode'
#
#  Input
#   x - numerischer Vektor
#
# Output
#   Varianz von x, numerischer Skalar
textBook <- function(x) {
  return((sum(x^2) - sum(x)^2 / length(x)) / (length(x) - 1))
}

## c)
# Nach gnaz viel Rechnen: n (n + 1) / 12
trueVar <- function(n) {
  return(n * (n + 1) / 12)
}


## d)
# Simulation fuer ein n und eine Wiederholung
oneSim <- function(n) {
  x <- sample(n)
  data.frame(
    solver = c("twoPass", "textBook", "var"),
    error = abs(c(twoPass(x), textBook(x), var(x)) - trueVar(n)),
    n = log2(n)
  )
}

# Wiederholungen machen
doReps <- function(n, repls) {
  do.call(rbind, replicate(repls, oneSim(n), simplify = FALSE))
}


doSim <- function(ns) {
  res <- do.call(rbind, lapply(ns, doReps, repls = 10))
  res$n <- as.factor(res$n)
  res
}
res <- doSim(2^(1:26))

ggplot(res, aes(x = n, y = error)) + geom_boxplot() + facet_grid(solver ~ .) +
  xlab("log2(n)")

# textBook geht als erstes kaputt und macht auch die groessten Fehler
# var und twoPass halten etwas laenger durch und machen anscheinend
# die gleichen Fehler. R scheint twoPass zu machen.
# Reihenfolge der Beobachtungen scheint keinen grossen Einfluss zu haben.



## Aufgabe 3

# youngsCramerVar - Varianzberechnung nach Youngs und Cramer
#
# Input
#   x - numerischer Vektor, dessen Varianz bestimmt werden soll
#
# Output
#  numerischer Skalar, die berechnete Varianz

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



testYC <- function() {
  testOneVar <- function(x) {
    expect_equal(youngsCramerVar(x), var(x))
  }
  
  for (n in 10^(1:3)) {
    testOneVar(1:n)
    testOneVar(rnorm(n))
    testOneVar(runif(n))
  }
}

testYC()


