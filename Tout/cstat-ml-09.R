## Aufgabe 1

## a) 

## golden_section_search - Golden Section Suche zur Bestimmung des Minimums
##                         einer Funktion
##
## Eingabe:
##  - f zu minimierende Funktion
##  - interval - numerisch, Laenge 2. Suchintervall.
##  - tau - numerisch, Praezisionsparameter, positiv.
##
## Ausgabe: Liste mit dem Median und der Anzahl benoetigter Iterationen

goldenSectionSearch <- function(f, interval, tau, Aufgabe.b = FALSE) {
  
  # Grenzen sind fuer dieses Problem einfach
  beta.lower <- interval[1]
  beta.upper <- interval[2]
  
  # erstes beta.best
  phi <- (sqrt(5) - 1) / 2
  beta.best <- beta.upper - phi * (beta.upper - beta.lower)
  
  i <- 0
  while(abs(beta.upper - beta.lower) >= tau * (1 + abs(beta.best))) {
    i <- i + 1
    beta.candidate <- beta.lower + (beta.upper - beta.best)
    if (f(beta.best) > f(beta.candidate)) {
      swap <- beta.candidate
      beta.candidate <- beta.best
      beta.best <- swap
    }
    if (beta.best < beta.candidate) {
      beta.upper <- beta.candidate
    } else {
      beta.lower <- beta.candidate
    }
    # Eine kleine Ergaenzung fuer Aufgabe b). Wir sparen uns viele Iterationen.
    if(Aufgabe.b){
      if(beta.upper - beta.lower <= 1)
        return(list(lower = beta.lower, best = beta.best, 
                    upper = beta.upper, it = i))
    }
  }
  return(list(opt = 0.5 * (beta.lower + beta.upper), it = i))
}

# und fuer b) schreiben wir eine kurze Wrapper-Funktion:

gss_median <- function(n){
  # (in a) waere die Eingabe noch von der Art wie f unten)
  # Zielfunktion definieren
  x <- 1:n
  f <- function(beta)
    sum(abs(beta - x))
  # Grenzen sind fuer dieses Problem einfach
  beta.lower <- 1
  beta.upper <- n
  if(beta.lower == beta.upper)
    return(list(median = beta.lower, iters = 0))
  # Hier greift dann Aufgabe.b, unser separates Abbruchkriterium.
  res <- goldenSectionSearch(f, interval = c(beta.lower, beta.upper), 
                             tau = 1e-12, Aufgabe.b = TRUE)
  # Nachbereitung, weil wir ja wissen, wir der Median definiert ist:
  # Median anders je nachdem ob Input gerade / ungerade
  if (n %% 2 == 1) {
    return(list(median = ceiling(res$lower), iters = res$it))
  } else {
    return(list(median = 0.5 + floor(res$best), 
                iters = res$it))
  }
}

gss_median(8)

testMyGS <- function() {
  for (n in 1:1e3) {
    expect_equal(gss_median(n)$median, (n + 1) / 2)
  }
}
testMyGS()


######### 
## Alternative (alte) Loesung ohne Wrapfunktion, die a) etwas ueberspringt:
#########

## golden_section_search - Golden Section Suche zur Bestimmung des Medians
##                         eines Vektors der Art 1, ..., n
##
## Eingabe:
##  n - ganze Zahl, zur Konstruktion des Vektors 1, ..., n 
##
## Ausgabe: Liste mit dem Median und der Anzahl benoetigter Iterationen

goldenSectionSearch <- function(n) {
  # (in a) waere die Eingabe noch von der Art wie f unten)
  # Zielfunktion definieren
  x <- 1:n
  f <- function(beta)
    sum(abs(beta - x))
  
  # Grenzen sind fuer dieses Problem einfach
  beta.lower <- 1
  beta.upper <- n
  
  # erstes beta.best
  phi <- (sqrt(5) - 1) / 2
  beta.best <- beta.upper - phi * (beta.upper - beta.lower)
  
  i <- 0
  # Sobald der Abstand nur noch 1 betraegt wissen wir nach Definition unserer
  # Vektors (und des Medians), dass wir den Median gefunden haben.
  while(beta.upper - beta.lower > 1) {
    # (in a) waere hier noch ein entsprechender Ausdruck auch in Abhaengigkeit
    # von beta.best und einem einzugebendem tau)
    i <- i + 1
    beta.candidate <- beta.lower + (beta.upper - beta.best)
    if (f(beta.best) > f(beta.candidate)) {
      swap <- beta.candidate
      beta.candidate <- beta.best
      beta.best <- swap
    }
    if (beta.best < beta.candidate) {
      beta.upper <- beta.candidate
    } else {
      beta.lower <- beta.candidate
    }
  }
  
  ## Median anders je nachdem ob Input gerade / ungerade
  if (n %% 2 == 1) {
    return(list(median = ceiling(beta.lower), iters = i))
  } else {
    return(list(median = 0.5 + floor(beta.best), iters = i))
  }
}
goldenSectionSearch(8)

testMyGS <- function() {
  for (n in 1:1e3) {
    expect_equal(goldenSectionSearch(n)$median, (n + 1) / 2)
  }
}
testMyGS()


# c)

plotIt2 <- function() {
  # exponentiell wachsende n untersuchen
  ns <- round(2^seq(0, 20, length.out = 128))
  iters <- sapply(ns, function(n) goldenSectionSearch(n)$iters)
  plot(ns, iters, log = "x", type = "b")
}
plotIt2()
# Linear bei logarithmierter x-Achse heisst logarithmischer Zusammenhang
# zwischen n und der Zahl der Iterationen.
# In jeder Iteration findet aber eine Auswertung der Zielfunktion statt, welche
# einen Aufwand in O(n) hat (eine Operation wird auf jedes Datum angewendet).
# Damit liegt die Laufzeit insgesamt in O(n * log n). Dies ist die selbe
# Groessenordnung wie beim vollstaendigen Sortieren der Daten.
# Das partielle Sortieren bei der ueblichen Median-Berechnung hat aber eine
# in der Groessenordnung geringe Laufzeit, weshalb sich die Verwendung der
# Golden-Section-Suche selbst bei dieser Art von Daten nicht lohnt.



## Aufgabe 2: 

# quadInterSearch - Quadratic Interpolation Search
#
# Eingabe:
#  f           - Funktion (numerischer Wert -> numerischer Wert),
#                die minimiert wird
#  theta.left  - Linker Rand des Suchintervalls, numerischer Wert
#  theta.right - Rechter Rand des Suchintervalls, numerischer Wert
#  tau         - Schranke fuer den relativer Fehler, numerischer Wert
#
# Ausgabe:
#  der gefunde optimale Parameterwert, numerischer Wert
quadInterSearch <- function(f, theta.left, theta.right, tau = 1e-4) {
  # initial best solution between lower and uppe
  theta.best <- (theta.left + theta.right) / 2
  
  # make and save f.evals
  f.best <- f(theta.best)
  f.right <- f(theta.right)
  f.lower <- f(theta.left)
  
  # Function to calculate new candidate
  getNewCandi <- function() {
    enum <- f.right * (theta.left^2 - theta.best^2) +
      f.best * (theta.right^2 - theta.left^2) + 
      f.lower * (theta.best^2 - theta.right^2)
    
    denom <- f.right * (theta.left - theta.best) +
      f.best * (theta.right - theta.left) + 
      f.lower * (theta.best - theta.right)
    
    enum / denom / 2
  }
  
  # optimization loop
  repeat {
    # calc new candidate
    theta.candidate <- getNewCandi()
    f.candi <- f(theta.candidate)
    
    # normal stop
    if (abs(theta.right - theta.left) < tau * (1 + abs(theta.best)))
      return(theta.best)
    
    # some special stopping criterias
    if (theta.best == theta.candidate || theta.candidate <= theta.left ||
        theta.candidate >= theta.right) {
      warning("Special stopping criteria applied. Perhaps something went wrong")
      return(theta.best)
    }
    
    # swap best and candidate? Also swap function values!
    if (f.candi < f.best) {
      swap <- theta.candidate
      theta.candidate <- theta.best
      theta.best <- swap
      swap <- f.candi
      f.candi <- f.best
      f.best <- swap
    }
    
    # replace one bound with the candidate
    if (theta.candidate < theta.best) {
      theta.left <- theta.candidate
      f.lower <- f.candi
    } else {
      theta.right <- theta.candidate
      f.right <- f.candi
    }
  }
  return(theta.best)
}



testQuadInt <- function(){
  f1 <- function(x) x^2
  f2 <- function(x) cos(x)
  f3 <- function(x) abs(x + 5)
  f4 <- function(x) x^2 + x
  
  expect_equal(quadInterSearch(f1, -10, 15), 0)
  expect_equal(quadInterSearch(f2, 1, 5), pi, tolerance = 1e-7)
  expect_equal(quadInterSearch(f3, -10, 15), -5, tolerance = 1e-7)
  expect_equal(quadInterSearch(f4, -10, 15), -0.5)
  
}