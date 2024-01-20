## Aufgabe 1

# myEvolution - Optimierung einer Funktion durch (mu + 1)-ES
#
# Input
#   f [function] - Zu minimierende Funktion von R^d nach R
#   lower [numeric(d)] - d-dimensionale untere Grenze des Suchraums
#   upper [numeric(d)] - d-dimensionale obere Grenze des Suchraums
#   mu [integer(1)] - Groesse der Startpopulation
#   lambda [integer(1)] - Anzahl der Nachkommen
#   T [integer(1)] -  Anzahl Iterationen
#
# Output:
#  Benannte Liste mit 2 Elementen
#   par: numeric(d), Koordinaten im Suchraum des gefundenen Minimums
#   val: numeric(1) Skalar, Zielfunktion von var
myEvolution <- function(f, lower, upper, mu, lambda, K) {
  d <- length(lower)
  # each col is one individual
  population <- matrix(runif(d * mu, lower, upper), ncol = mu)
  
  for(t in 1:K) {
    # generate new children
    parentInds <- replicate(2, sample(mu, lambda, replace = TRUE))
    # uniform recombination:
    children <- apply(parentInds, 1, function(p) {
      apply(population[, p, drop = FALSE], 1, function(x) sample(x, 1))
    }) 
    # mutation: 
    children <- children + rnorm(d * lambda, mean = 0, sd = 1 - 0.9 * t / K)
    # put child back into definition space
    children <- pmax(children, lower)
    children <- pmin(children, upper)
    
    # select next population
    children <- matrix(children, ncol = lambda) # Only relevant for case d = 1
    fitness <- apply(children, 2, f)
    population <- children[, order(fitness)[1:mu], drop = FALSE]
  }
  best <- which.min(fitness[order(fitness)[1:mu]])
  return(list(par = population[, best], val = fitness[best]))
}

testMyES <- function() {
  # Test auf Einheitssphaere und Analyse des Einflusses von d
  for(d in 1:3) {
    f <- function(x) drop(crossprod(x))
    print(paste("d =", d, " mean value:",
                mean(replicate(10, (myEvolution(f, rep(-5, d),
                                                rep(5, d), 10 * d,
                                                15 * d, 1000 * d))$val))))
  }
}
testMyES()
# "d = 1  mean value: 0.0190037336310981"
# "d = 2  mean value: 0.0318135689659988"
# "d = 3  mean value: 0.0842715052497615"
# "d = 5  mean value: 0.142187398364789"
# "d = 10  mean value: 0.509897420038482"
# Mit steigender Dimension wird das Ergebnis immer schlechter.
# Curse of Dimensionality

## Aufgabe 2

## Daten erzeugen
set.seed(2)
mu1 <- 4
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
pi <- 0.3
n <- 30
y <- sample(round(sample(c(rnorm(pi * n, mu1, sigma1),
                          rnorm((1 - pi) * n, mu2, sigma2))), 2))


## a)

# myEMAlgo - fuehrt dem EM-Algorithmus aus dem Skript aus
# Input
#  y - numerischer Vektor, die Beobachtungen
# Output
#  benannte Liste mit den Elementen
#   theta - numerischer Vektor, die geschaetzten Params
#   likeli - numerischer Vektor, Verlauf der Likelihood
#   gamma - numerischer Vektor, finale responsibilities
#
myEMAlgo <- function(y) {
  # Waehle zufaellige Startwerte
  pi <- runif(1)
  mu1 <- rnorm(1)
  mu2 <- rnorm(1)
  sigma1 <- 0.5 + rexp(1)
  sigma2 <- 0.5 + rexp(1)
  
  gamma <- rep(Inf, length(y))
  likeli <- prod((1 - pi) * dnorm(y, mu1, sigma1) + pi * dnorm(y, mu2, sigma2))
  
  repeat {
    # E-Schritt
    gamma.alt <- gamma
    gamma <- pi * dnorm(y, mu2, sigma2) /
      ((1 - pi) * dnorm(y, mu1, sigma1) + pi * dnorm(y, mu2, sigma2))
    
    # M Schritt
    mu1 <- sum((1 - gamma) * y) / sum(1 - gamma)
    mu2 <- sum(gamma * y) / sum(gamma)
    sigma1 <- sqrt(sum((1 - gamma) * (y - mu1)^2) / sum(1 - gamma))
    sigma2 <- sqrt(sum(gamma * (y - mu2)^2) / sum(gamma))
    pi <- sum(gamma) / length(y)
    
    
    likeli <- c(likeli, 
               prod((1 - pi) * dnorm(y, mu1, sigma1) + pi * dnorm(y, mu2, sigma2)))
    
    # Wenn eines der Sigmas 0 wird haben wir verloren
    if (any(c(sigma1, sigma2) < 1e-6))
      return(list(theta = theta, likeli = likeli, gamma = gamma))
    
    if (sum(abs(gamma - gamma.alt)) < 1e-6)
      break
  }
  
  theta <- c(pi, mu1, mu2, sigma1, sigma2)
  
  return(list(theta = theta, likeli = likeli, gamma = gamma))
}

## b)
set.seed(1)
res <- replicate(100, myEMAlgo(y), simplify = FALSE)
thetas <- t(sapply(res, function(x) x$theta))
table(round(thetas, 2)[, 1])
# Wir sehen 4 Optima, von denen jeweils 2 durch Vertauschung
# der Klassen zueinander passen. Zum Beispiel:
# pi1 = 0.21 und 0.79 passen zueinander
# pi1 = 0.27 und 0.73 passen ebenfalls
# mu1/2 und sigma1/2 sind dann jeweils vertauscht

# Da sie am haeufigsten auftreten, scheinen diese beiden die besten Kandidaten
# zu sein:
# 0.7316850 4.3415976 0.3595595 1.0023246 1.3622735
# 0.7852498 0.3257515 1.7294431 0.4518117 2.3575145
# Da das ein wenig haeufiger vorkommt, koennte das besser
# sein, vielleicht hat es aber nur ein groesseres Einzugsgebiet. Wahrheit kann
# nur die Likeliehood bringen:
likeli <- sapply(res, function(x) x$likeli)
# Wir schauen examplarisch auf den 3. und 6. Run
likeli[c(5, 7)]
# Wir wollen die Likelihood ja maximieren, daher scheint der 7. besser zu sein
# Das ist der mit pi1 = 0.73
thetas[7, ]

## c)
mean(sapply(likeli, length))
# Knapp 105
# Das haengt dann aber ganz stark vom gewaehlten Abbruchkriterium und auch
# vom jeweiligen Optimum ab
summary(sapply(likeli, length))

# d)
# Wir suchen uns einen Lauf raus, der das richtige Optimum findet
plot(res[[7]]$likeli, type = "b", log = "y", ylab = "Likelihood", xlab = "Iter")
# Scheint ein guter Startwert zu sein, am Anfang viel Fortschritt und dann
# langsam ins Optimum, aber iter 60 fast gar kein fortschritt mehr. wir koennten
# also wahrscheinlich auch frueher stoppen

plot(y, res[[7]]$gamma, ylab = "gamma", xlab = "Beobachtung", pch = 16)
# Man sieht schoen die Grenze zwischen den beiden "Klassen"



## Aufgabe 3

# numericInvert - Invert eine Matrix A n mal.
#
# Input:
#   A - Numerische Matrix
#   n - Ganze Zahl
#
# Output
#  numerische Vektor, mit 3 Elementen: 
#   mean.error - mittlere Differenz zwischen A und n mal invertiertem A.
#   norm.f     - Frobeniusnorm von A
#   norm.s     - Spektralnorm von A
numericInvert <- function(A, n) {
  A.inv <- A
  for (i in 1:n) {
    A.inv <- solve(A.inv)
  }
  mean.error <- mean(abs(A - A.inv))
  norm.f <- sqrt(sum(A^2))
  norm.s <- eigen(A %*% t(A))$values[1]
  return(c(
    mean.error = mean.error,
    norm.f = norm.f,
    norm.s = norm.s
  ))
}

## b)
# Funktion zur Durchfuehrung der kleinen Studie
doitA3 <- function(n = 2L) {
  A <- matrix(c(
    9, 9, 8,
    9, 8, 8,
    8, 8, 7),
    ncol = 3, byrow = TRUE)
  
  as <- as.vector(sapply(2:6, function(k) 1:9 * 10^k - 10))
  as <- c(10 * 0:8, as, 9999990)
  res <- sapply(as, function(a) numericInvert(a + A, n))
  opar <- par(mfrow = c(3L, 1L))
  plot(as + 1 , res[1L, ], type = "b", log = "xy", main = "mittlerer Fehler")
  plot(as + 1, res[2L, ], type = "b", log = "xy", main = "Frobeniusnorm")
  plot(as + 1, res[3L, ], type = "b", log = "xy", main = "Spektralnorm")
  par(opar)
}

doitA3(2L)
# Fehler wird groesser, ungefaehr im gleichen Rahmen in dem auch die Norm
# steigt
doitA3(10L)
# Keine grosse Aenderung

