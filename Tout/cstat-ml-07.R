source("LCF.R")
library(e1071)
load("zufall.RData")

## Aufgabe 1

# Erweiteret Euklidischer Algorithmus
#
# Input:
#   a, b: Ganze Zahlen mit a < b
#   
# Output: benannte Liste mit:
#   - gcd: groesster gemeinsamer Teiler, numeric(1)
#   - inv.elem: Das inverse Element, numeric(1)
extEucl <- function(a, b){
  u <- c(1, 0, a)
  v <- c(0, 1, b)
  while (v[3] != 0) {
    q <- floor(u[3] / v[3])
    tmp <- u - v * q
    u <- v
    v <- tmp
  }
  inv.el <- u[1]
  if (inv.el < 0)
    inv.el <- b + inv.el
  return(list(gcd = u[3], inv.el = inv.el))
}

# Testen
testMyExtEucl <- function() {
  singleTest <- function(x) {
    res <- extEucl(x[1], x[2])
    # Eigenschaft des Inversen Element ueberpruefen
    expect_equal((res$inv.el * x[1]) %% x[2], res$gcd)
  }
  
  apply(cbind(1:30, 31), 1, singleTest)
}


## ig - Implementation of inversive random number generator
#
# Input
#  n        - desired number of random numbers
#  seed     - seed
#  a, ce, m - parameters of the ig
#
# Outout
#  A vector of pseudo random numbers
ig <- function(n, seed, a, ce, m){
  x <- numeric(n + 1)
  x[1] <- seed
  for (i in 2:(n + 1)) {
    if (x[i - 1] != 0) {
      ## calculate x^{-1}
      x.inv <- extEucl(x[i - 1], m)$inv.el    
      ## now the inversive generator magic
      x[i] <- (a * x.inv + ce) %% m
    } else
      x[i] <- ce %% m
  }
  # Die erste Zahl ist keine ZZ, da sie dem seed entspricht.
  return(x[-1])
}

hist(ig(100000, 1273, 500, 26, 2^31 - 1), main = "Histogramm von IG ZZs",
     xlab = "x", freq = FALSE, breaks = 50)
abline(h = 1 / (2^31 - 1), col = "red", lwd = 2)
# Das schaut halbwegs gleichverteilt. Der rechte Rand sieht etwas kritisch aus.
# Ob die ZZs wirklich gut sind, wuerden die Tests aus A2 ueberpruefen koennen.


## Aufgabe 2
## a)

## permutationTest - fuehrt einen Permutationstest durch
##
## Eingabe:
##  u  - Vektor der Zufallszahlen aus dem Intervall [0, 1]
##  Te - analysiere die Permutation von Sequenzen der Laenge T, natuerliche Zahl
##
## Ausgabe:
##     der p-Wert des Tests

permutationTest <- function(u, Te) {
  browser()
  ## Anzahl der Gruppen:
  N <- length(u) %/% Te
  ## Daten in N Gruppen geteilt (ggf. muessen einige entfernt werden):
  groups <- matrix(u[1:(Te * N)], nrow = N, byrow = TRUE)
  ## Anordnung der Daten je Gruppe: Positionen
  obs.perms <- t(apply(groups, 1, order))
  ## ... zusammengefasst zu einem character je Gruppe:
  obs.perms <- apply(obs.perms, 1, function(x) paste(x, collapse = ""))
  ## welche Anordnungen der Daten in einer Gruppe sind moeglich? 
  ## (alle Permutationen der Zahlen von 1 bis Te, als character)
  possible.perms <- apply(permutations(Te), 1, function(x)
    paste(x, collapse = ""))
  ## ... davon bei den N Gruppen realisiert (als factor):
  obs.perms <- factor(obs.perms, levels = possible.perms)
  ## Haeufigkeiten der Anordnungen mit table(); 
  ## chi-quadrat-Test, ob alle alle Anordnungen gleich haeufig sind 
  ## (d.h. Anpassungstest auf diskrete Gleichverteilung ueber Te! Anordnungen) 
  ## (p-Wert unter Nullhypothese gleichverteilt)
  return(chisq.test(table(obs.perms))$p.value)
}

permutationTest(runif(100,0,1),5)


## b)

## maxTauTest - fuehrt den Gleichverteilungstest durch
##
## Eingabe:
##  u - Vektor der Zufallszahlen aus dem Intervall [0, 1]
##  tau - Ganzzahliger Skalar, zentraler Parameter des Tests
##  D - aus den Elementen aus u werden D ganzzahlige Werte konstruiert,
##      natuerliche Zahl, D < m (aus LCF)
##
## Ausgabe:
##   der p-Wert des Tests

maxTauTest <- function(u, tau, D) {
  browser()
  N <- length(u)
  ## Daten in tau-Tupel geteilt (ggf. muessen einige entfernt werden):
  tuples <- matrix(u[1:(N - N %% tau)], ncol = tau, byrow = TRUE)
  ## max()^tau je Tupel:
  z <- apply(tuples, 1, max)^tau
  ## ... auf Gleichverteilung testen: 
  ## (p-Wert unter Nullhypothese gleichverteilt)
  uniformDistrTest(z, 1, D)
}

maxTauTest(runif(100,0,1),5,32)


## c)

## Liste mit allen unseren Tests

tests <- list(
  max.tau.2 = function(u)
    maxTauTest(u, tau = 2, D = 32),
  max.tau.3 = function(u)
    maxTauTest(u, tau = 3, D = 32),
  max.tau.4 = function(u)
    maxTauTest(u, tau = 4, D = 32),
  max.tau.5 = function(u)
    maxTauTest(u, tau = 5, D = 32),
  perm.3 = function(u)
    permutationTest(u, Te = 3),
  perm.4 = function(u)
    permutationTest(u, Te = 4),
  perm.5 = function(u)
    permutationTest(u, Te = 5)
)

singleCheck <- function(u, splits = 1000, test.list = tests){
  ## Teile in Stuecke:
  u.splitted <- split(u, rep(1:splits, each = length(u) / splits))
  ## Pruefe in diesem Setting das Verhalten der Tests:
  res <- t(sapply(u.splitted, function(u) sapply(test.list, function(fun) fun(u))))
  ## (Histogramme sollten idealerweise Gleichverteilung (der p-Werte) zeigen)
  par(mfrow = c(3, 3))
  sapply(1:ncol(res), function(i){ 
    hist(res[, i], freq = FALSE, main = colnames(res)[i], xlab = "p Wert", 
         xlim = 0:1) 
    abline(h = 1, col = "red")
  })
  par(mfrow = c(1, 1))
}

# Gute Werte fuer a und c laut Hellekalek (1995): 
# a = 1288490188, c = 1
# a = 9102, c = 36884165
# a = 14288, c = 758634
# a = 21916, c = 71499791
# a = 28933, c = 59217914
# a = 31152, c = 48897674

u <- ig(1e6, 1273, 10, 10, 2^31 - 1) / (2^31 - 1)
u0 <- ig(1e6, 1234, 14288, 758634, 2^31 - 1)/ (2^31 - 1)
# Kurzer Test, dass gleichverteilt auf [0,1] hinkommt (nach Teilen durch 2^31 - 1) 
par(mfrow = c(2, 2))
hist(u, freq = FALSE)
hist(u0, freq = FALSE)
hist(u1, freq = FALSE)
hist(u2, freq = FALSE)
par(mfrow = c(1, 1))

# Jetzt die formalen Tests:
singleCheck(u)
singleCheck(u0)
# Sieht in beiden Faellen okay aus, da die p-Werte aller Tests ungefaehr gleich-
# verteilt auf [0,1] sind
singleCheck(u1)
# Die Permutationstests zeigen zu viele kleine p-Werte => keine guten ZZ.
# Was hier schief geht, ist dass die ZZ zu haeufig aufsteigend sortiert sind: 
plot(u1[1:1000])
singleCheck(u2)
# In diesem Fall schlaegt der Maximum-Tau-Test aus (insb. fuer groessere Tau). 
# Problem ist, dass die ZZ in der 2. Haelfte der Folge haeufiger grosse Werte 
# annehmen als in der 2. Haelfte: 
plot(u2[1:1000])

# Loesung alternativ mit runif() und LCF:
u <- runif(10e6)
u0 <- lcs(1e6, 1273, 12353243123, 453816693)/ 2^31
# Kurzer Test, dass gleichverteilt auf [0,1] hinkommt (nach Teilen durch 2^31 - 1) 
hist(u, freq = FALSE)
hist(u0, freq = FALSE)
# 2. Fall sieht schon nicht ganz so gut aus...
# Jetzt die formalen Tests:
singleCheck(u)
singleCheck(u0)
# runif() besteht die Tests, der LCF Generator nicht. a erfuellt die mod Anfor-
# derungen nicht.



## Aufgabe 3

## a)
## Poissonverteilung
## myRPois: Zufallszahlen-Generator fuer eine Poisson-Verteilung
##
## Eingabe:
##   n      - Anzahl zu erzeugender Zufallszahlen, natuerliche Zahl
##   lambda - Parameter der Poisson-Verteilung, Skalar > 0
##
## Ausgabe: Erzeugte Zufallszahlen, numeric(n)

myRPois <- function(n, lambda){
  # Auf 0,..., 2^(31 - 1) diskret gleichverteilte ZZ erzeugen aus den auf [0, 1]
  # stetig gleichverteilten ZZ (runif liefert stets Zahlen aus dem offenen Inter-
  # vall):
  z <- floor(runif(n) * 2^31)
  zz <- sapply(z, function(x){
    i <- 0
    repeat{
      # print((2^31 - 1) * sum(dpois(0:i, lambda)))
      if((2^31 - 1) * sum(dpois(0:i, lambda)) > x) return(i)
      i <- i + 1
    }
  })
  return(zz)
}
# Zeige, dass der Generator funktioniert:
par(mfrow = c(2, 2))
for(i in c(3, 6, 10, 15)){
  plot(table(myRPois(1e5, i)) / 1e5, xlab = "x", ylab = "Dichte", lwd = 4)
  points(0:35, dpois(0:35, i), type = "h", col = "red")
}
# In rot sind die theoretischen Dichten eingezeichnet. Diese liegen immer
# sehr nah an den empirischen Dichten.


## b)
## Exponentialverteilung
## Input
## n - ganze Zahl, wie viele Zufallszahlen
## rate - Reelle Zahl, Parameter der Verteilung
## Output
## Numerischer Vektor, Zufallszahlen aus der Verteilung
myRExp = function(n, rate) {
  - log(1 - runif(n)) / rate
}

par(mfrow = c(1, 1))
hist(myRExp(1000, 5), breaks = 20, freq = FALSE)
curve(dexp(x, 5), add = TRUE)
## Passt


## c) Binomialverteilung

## Dazu zuerst fuer die Bernoulli-Verteilung:

# myRBern: Zufallszahlen-Generator fuer eine Bernoulli-Verteilung
#
# Input:
#   n - Anzahl zu erzeugender Zufallszahlen, natuerliche Zahl
#   p - Erfolgswahrscheinlichkeit, Skalar im Intervall (0, 1)
#
## Output: Erzeugte Zufallszahlen, numeric(n)

myRBern <- function(n, p){
  z <- runif(n)
  zz <- as.numeric(z < p)
  return(zz)
}

sapply(1:9 / 10, function(p) mean(myRBern(1e6, p)) - p)
## sieht gut aus, kleine Abstaende sind zu erwarten.


# myRBinom: Zufallszahlen-Generator fuer eine Bernoulli-Verteilung
#
# Input:
#   n - Anzahl zu erzeugender Zufallszahlen, natuerliche Zahl
#   nb- Parameter n der Binomialverteilung
#   p - Erfolgswahrscheinlichkeit, Skalar im Intervall (0, 1)
#
## Output: Erzeugte Zufallszahlen, numeric(n)

myRBinom <- function(n, nb, p){
  replicate(n, sum(myRBern(nb, p)))
}

(x <- barplot(table(myRBinom(1000, 20, 0.2))/1000))
points(x, dbinom(0:(length(x)-1), size = 20, prob = 0.2), col = "red", type = "h")
## sieht gut aus.

## d) 
## Dreiecksverteilung
# myRDrei: Zufallszahlen-Generator fuer eine Dreiecksverteilung
#
# Eingabe:
#   n       - Anzahl zu erzeugender Zufallszahlen, natuerliche Zahl
#   a, b, H - Parameter der Verteilung, jeweils Skalare mit a < H < b
#
# Ausgabe: Erzeugte Zufallszahlen, numeric(n)

myRDrei <- function(n, a, b, H) {
  u <- runif(n, 0, 1)
  ifelse(u < (H - a) / (b - a),
         sqrt(u * (b - a) * (H - a)) + a,
         b - sqrt((1 - u) * (b - a) * (b - H)))
}

hist(myRDrei(1e5, 2, 4, 3.5), freq = FALSE, xlab = "x",
     main = "Histogram Dreieck", breaks = 50)
x <- seq(2, 4, length.out = 127)
y <- ifelse(x < 3.5, (x - 2) / 1.5, (4 - x) / 0.5)
lines(x, y, lwd = 2, col = "red")


