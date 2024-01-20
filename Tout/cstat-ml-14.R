library(ggplot2)
library(testthat)
source("sim.R")
source("greville.R")

## Aufgabe 1

## b=Vektor passender Laenge
##
## Eingabe:
##  R - obere Dreiecksmatrix der Dimension n x n
##  b - Vektor der Länge n
##
## Ausgabe:
##  x - die Lösung des Gleichungssystems Rx=b
##
## Beschraenkungen:
##  keine Fehlerprüfung
my_backsolve <- function(R,b) {
  n <- ncol(R)
  x <- numeric(n)
  
  x[n] <- b[n] / R[n, n]
  if (n > 1) {
    for (i in (n-1):1) {
      ri <- R[i, ]
      x[i] <- (b[i] - crossprod(x, ri)) / ri[i]
    }
  }
  x
}

test_my_backsolve <- function() {
  library(testthat)
  
  test_it <- function(R) {
    b <- runif(nrow(R))
    x <- my_backsolve(R, b)
    expect_true(is.vector(x))
    expect_equal(drop(R %*% x), b)
  }
  
  test_that("1x1 R", {
    test_it(matrix(1, nrow=1))
    test_it(matrix(2, nrow=1))
    test_it(matrix(-1, nrow=1))
  })
  
  test_that("random diagonal R", {
    set.seed(42)
    for (dim in 1:10) {
      test_it(diag(runif(dim), nrow=dim))
    }
  })
  
  test_that("random R", {
    set.seed(42)
    for (dim in 1:10) {
      R <- matrix(runif(dim * dim), nrow=dim)
      R[lower.tri(R)] <- 0
      test_it(R)
    }
  })
}


## Aufgaben 2 + 3

# z1ReduceRank - reduziert den Rang einer Zielke Testmatrix
#
# Input
#  X - numerische Matrix, die Zielke Matrix
#  Xinv - numerische Matrix, exakte Inverse von Z
#  k - gewuenschter Spalten-Rang der reduzierten Matrix
#
# Output
#  Benannte Liste mit 2 Elementen:
#   X - numerische Matrix, rangreduzierte Version von Z
#   Xinv - numerische Matrix, exakte Inverse von X
z1ReduceRank <- function(X, Xinv, k) {
  
  stopifnot(all(dim(X) == dim(Xinv)),
            ncol(X) >= 4,
            ncol(X) %% 2 == 0)
  n <- nrow(X)
  out <- -seq(2, n - 2, 2)[1:k]
  W <- wMatrix(n)
  
  list(
    X = t(X[out, ]),
    Xinv = t(W %*% Xinv[, out])
  )
}

# doTest - Fuehrt die Simulation fuer ein Testproblem und einen Solver aus
#
# Input
#  tst - Ein Testproblem, wie von z1ReduceRank erzeugt
#  lhs - Linke Seite, wie von leftHandSide erzeugt
#  solver - ein KQ solver wie oben definiert
#
# Output
#  Relativer Fehler
doTest <- function(tst, lhs, solver) {
  res <- try({
    beta.est <- solver(tst$X, lhs$y)
    res <- beta.est - lhs$beta
    sqrt(diag(crossprod(res))) / sqrt(diag(crossprod(lhs$beta)))
  })
  if(inherits(res, "try-error"))
    rep(NA, ncol(lhs$beta))
  else
    res
}

# simulation - fuehre die Simulationsstudie aus
#
# Input
#  Asizes - Ganzzahliger Vektor, Groessen der Matrizen
#  Zvalues - Ganzzahliger Vektor, Kondition der Matrizen
#  solver - Liste von Funktionen, jeweils KQ-Solver wie weiter oeben
#
# Output
#  Dataframe mit den Ergebnissen
simulation <- function(Asizes, Zvalues, solvers) {
  res <- NULL
  for (Z in Zvalues) {
    for (n in Asizes) {
      k <- n / 2 - 1    
      tst <- z1ReduceRank(zielkeMatrix(Z, n), invZielkeMatrix(Z, n), k)
      lhs <- leftHandSide(tst$X, tst$Xinv)
      cond <- sqrt(sum(tst$X * tst$X)) * sqrt(sum(tst$Xinv * tst$Xinv))
      
      for (solver_name in names(solvers)) {
        solver <- solvers[[solver_name]]
        tmp <- data.frame(solver = solver_name,
                         Z = Z, n = n, k = k, phi = lhs$phi, cond = cond,
                         error = doTest(tst, lhs, solver))
        res <- rbind(res, tmp)
      }
    }
  }
  res
}

# llsPlots - erzeugt Ergebnisgraphiken fuer die Studie
#
# Input
#   results - Ergebnis-Dataframe
#
# Output
#  ggplot-Objekt, enthaelt den Plot
llsPlots <- function(results) {  
  results <- transform(results,
                      error = ifelse(is.na(error), 1e8, error))
  results$type <- c("y1 (sehr kleiner Winkel)", "y2 (etwas groesserer Winkel)",
                   "y3 (mittelgrosser Winkel)", "y4 (sehr grosser Winkel)")
  ggplot(results, aes(x = cond, y = error, colour = solver)) +
    geom_point(position = position_jitter(w = 0.2, h = 0.2)) + 
    scale_x_log10() + scale_y_log10() + 
    geom_smooth(data = results, method = "lm", se = FALSE) + 
    facet_wrap(~type)
}


## plotIt - Simulation durchfuehren und Plots machen
#  save - sollen die Daten ein einer RData abgespeichert werden?
plotIt <- function(save = FALSE) {
  solvers <- list(
    normal = lsSolveNormal,
    lm = lsSolveLM,
    mgs = lsSolveMGS,
    greville = lsSolveGreville
  )
  
  data <- simulation(Asizes = c(4, 8, 16, 32, 64),
                    Zvalues = 2^(0:20),
                    solvers = solvers)
  
  if (save) {
    save(data, file = "sim_res.RData")
  }
  
  print(llsPlots(data))
}

plotIt()

# Die Ergebnisse fuer die ersten drei linken Seiten sind sehr aehnlich. Es ist zu 
# erkennen, dass MGS und lm hier ungefaehr gleich gut sind, lm aber ab Kondi-
# tionszahlen groesser als 10^8 keine Ergebnisse liefert. Greville und Normalen-
# gleichung sind etwa gleich gut, aber deutlich schlechter als MGS und lm. Fuer 
# die letzte linke Seite, welche die mit dem groessten Winkel ist, ist bis zu  
# einer Konditionszahl von ca. 10^5 die Loesung mittels Normalengleichung am  
# besten und alle weiteren Verfahren etwa gleich gut. Danach bleiben Normalen- 
# gleichung und Greville, wie bei den anderen y auch, konstant bei einem relati-
# ven Fehler von 1, lm liefert keine Loesung mehr und die relativen Fehler beim 
# MGS streuen sehr stark. Die Ergebnisse aus der Vorlesung koennen somit bestae-
# tigt werden.
