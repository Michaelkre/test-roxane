load("logreg_exp.Rdata")

library(mvtnorm)
library(testthat)

## Aufgabe 1

## 1. Schritt: Mal in die Daten hineinschauen, wie sehen die ueberhaupt aus?
summary(data)
# Hey, wir haben ja NA's, cool. Schauen wir mal erstmal an, welche das sind.
summary(data[is.na(data$suboptimality), ])
# Anscheinend im Gradient descent, meistens bei b = 1000, ein paar mal auch
# bei b = 500. Und zwar kommt der NA immer nach 21 fevals. Hier waere die
# Fehlermeldung jetzt mal echt interessant, die haben wir aber leider nicht.
# Zu den anderen Einflussparametern scheint es keinen Zusammenhang
# zu geben. Anscheinend hat der GD-Algorithmus hier ein Problem, mehr koennen
# wir leider nicht sagen, das muessen wir fuer spaeter im Hinterkopf behalten.

table(data$algorithm, data$b)
table(data[is.na(data$suboptimality), "b"])
# Von 256 Durchlaeufen sind 72 mit einem Fehler abgebrochen. Das ist schon
# ziemlich viel.
# Stoeren uns erstmal nicht weiter, werden in den Plots nicht angezeigt,
# muessen wir aber im Hinterkopf behalten.
na.inds <- is.na(data$suboptimality)

# Ansonsten ist erstmal nichts besonderes an den Eingabedaten zu sehen -
# die Einstellungsparameter sind auf dem Gitter veraendert (-> vollfaktorielles
# Design) und den Einfluss zu den Messgroessen untersuchen wir jetzt.
# Was vielleicht noch auffaellig ist: Die Messgroessen sind sehr schief
# verteilt. Wir sollten hier gegebenenfalls auf logarithmierte Skalen wechseln!

# Schauen wir abschliessend vielleicht noch einmal auf die Unterschiede der
# 3 Algorithmen:
splitted <- split(data, data$algorithm)
attach(splitted)
summary(bfgs)
summary(newton)
summary(gradient_descent)

# Vor allem die grossen Unterschiede in 'neval' fallen uns hier auf.
# Newton und BFGS haben ein max von 50 / 149, GD braucht bis ueber 4000.
# Dafuer erreicht GD immer eine perfekte 'suboptimality', jedoch ein recht
# hohes 'distance to optimum'.
# Hier scheint es wesentliche Unterschiede zwischen den Algos zu geben.

# Sehr interessant waere hier zu wissen, mit welchem Abbruchkriterium die
# Algorithmen jeweils gearbeitet haben. Wissen wir aber leider nicht...


# 1. Frage: Einfluss von 'b', 'n', 'kappa', 'epsilon' auf die Zielgroessen.
# Machen wir mal ein paar Boxplots und schauen, was dabei herauskommt.
# Boxplots, weil wir ja von den Einflussgroessen immer nur 4 verschiedene
# Einstellungen haben. Scatterplots wuerden daher nicht gut aussehen.

par(mfcol = c(3, 4))
for (x in c("b", "n", "kappa", "epsilon")) {
  for (y in c("suboptimality", "distance_to_optimum", "n_eval")) {
    plot(as.factor(data[, x]), data[, y], xlab = x, ylab = y)
  }
}

# Seien wir ehrlich - wir sehen gar nichts, weil die Daten viel zu schief
# sind. Also logarithmieren. Trick: Wir addieren 1e-8, da manche Werte 0
# sind und log(0) natuerlich nicht so ganz plotten kann.

makeLogBoxPlots <- function(d) {
  par(mfcol = c(3, 4))
  for (x in c("b", "n", "kappa", "epsilon")) {
    for (y in c("suboptimality", "distance_to_optimum", "n_eval")) {
      plot(as.factor(d[, x]), d[, y] + 1e-8, xlab = x, ylab = y, log = "y")
    }
  }
}
makeLogBoxPlots(data)

# Dann versuchen wir mal zu verstehen, was wir hier sehen:
#
# 1. Zeile: 'suboptimality'. Viel Einfluss ist nicht zu sehen, das Bild wird
# vor allem von den vielen, vielen Ausreissern beeinflusst. Wenn man die
# Mediane der Boxplots verfolgt, kommt man aber zu dem Schluss: Bei steigendem
# 'n' und 'kappa' sinkt die 'suboptimality'. Wenn 'b' (Dimension) und 'kappa'
# (Kondition) steigen, wird das Problem natuerlich schwerer. Dann ist auch die
# gefundeneLoesung am Ende schlechter. Nachvollziehbar. Ein hohes 'b'
# (Beobachtungsanzahl) scheint das Problem hingegen leichter zu machen. Je mehr
# Beobachtungen vorliegen, desto "schlechter" ist die lineare Trennbarkeit,
# weil die W'keit fuer extreme Beobachtungen steigt. Man kann sich also
# vorstellen, dass das Problem bei steigendem 'b' leichter wird. Zeigt sich
# hier. 'epsilon' hat keinen Einfluss.
#
# 2. Zeile: 'Distance to optimum'. Hier sieht man eigentlich genau das
# gleiche wie bei 'suboptimality', nur noch ein wenig deutlicher: 'b' macht
# das Problem leichter, 'n' und 'kappa' machen es schwerer, d.h. erzeugen eine
# schlechtere Loesung, und 'epsilon' hat keinen Einfluss.
#
# 3. Zeile: 'n_eval'. Hier hat 'b' (nahezu) keinen Einfluss. 'n' und 'kappa'
# machen das Problem zwar offensichtlch schwerer, jedoch spiegelt sich das hier
# anders wieder: Hohes 'n' fuehrt zu mehr 'f_evals', hohes 'kappa' aber
# anscheinend nicht. 'epsilon' ist weiterhin ohne Einfluss.

# Insgesamt hat 'epsilon' ueberhaupt keinen Einfluss - wie erwartet, die
# Richtung, in die wir vom Startpunkt aus entfernt sind, hat keinen Einfluss.
# Die Beobachtungszahl 'b' macht das Problem ein wenig leicher, waehrend
# steigendes 'n' und 'kappa' eine laengere Optimierung und ein schlechteres
# Ergebnis zur Folge haben.

# Ein kleiner Blick getrennt nach Algorithmen, vielleicht ergibt sich hier
# ja ein anderes Bild:

makeLogBoxPlots(bfgs)
makeLogBoxPlots(newton)
makeLogBoxPlots(gradient_descent)

# Interessant: Der Einfluss von 'b' auf 'suboptimality' und 'distance_to_opt'
# ist eigentlich nur beim Gradient Descent vorhanden. Der Einfluss auf
# von b au fneval ist bei BFGS positiv, bei Gradient Descent negativ.
# 'epsilon' hat bei Newton ein Einfluss auf 'n_eval', ansonsten hat 'epsilon'
# wie bereits festgestellt nie einen Einfluss.
# Viele Tendenzen von oben finden sich hier (natuerlich) wieder, im Detail
# haben die Algorithmen aber deutlich individuelle Verhaltensweisen.


## 2. Frage: Welcher Algorithmus verwendet die geringste Anzahl an f.evals?

par(mfrow = c(1, 1))
boxplot(split(data$n_eval, data$algorithm), log = "y",
        ylab = "nevals", main = "Vergleich benoetigter f.evals")

# Keine grosse Ueberraschung - Newton gewinnt. Newton hat eine Hesse-Matrix
# und somit am meisten Information, braucht also am wenigsten evals.
# Dann kommt BFGS - BFGS approximiert die Hesse-Matrix, dann GD, GD
# benutzt keine Hesse-Matrix.


## 3. Frage: Ist ein Algorithmus besser als ein anderer?

boxplot(split(data$suboptimality + 1e-8, data$algorithm),
        ylab = "suboptimality", main = "Vergleich suboptimality", log = "y")

# Newton ist hier eigentlich immer auf 1e-8, was aufgrund unserer Addition
# der 0 enspricht, die anderen beiden liegen deutlich darueber.

boxplot(split(data$distance_to_optimum, data$algorithm), log = "y",
        ylab = "distance_to_optimum", main = "Vergleich distance_to_optimum")

# Und hier sehen wir an sich das gleiche Bild.

# Newton benoetigt auch noch weniger f.evals. Also immer Newton nehmen?
# Wenn man denn eine Hesse-Matrix vorliegen hat. Das ist jedoch meistens nicht
# so. Ausserdem wird hier auch der Aufwand, der durch die zusaetzliche
# Auswertung der Hesse-Matrix entsteht, ignoriert.
# Naechste Wahl waere BFGS. Man sieht: BFGS kann "schiefgehen" und relativ
# weit vom Optimum entfernt enden. Dafuer ist es aber auch wesentlich schneller
# als GD.
# Nicht vergessen darf man hier die NAs, die GD manchmal hat. GD scheint also
# raus zu sein. Fuer Newton oder BFGS muesste man noch mal auf den Aufwand
# fuer die Auswertung der Hesse-Matrix schauen.


## 4. Frage: Korreliert die Anzahl f.eval mit der Guete der Loesung?

# Schauen wir uns erstmal 'suboptimality' an.
plot(data$n_eval, data$suboptimality, ylab = "suboptimality", xlab = "nevals",
     main = "Korrelation n_eval und suboptimality")
# Wir sehen nichts. Mal logarithmieren.

plot(data$n_eval, data$suboptimality + 1e-8,
     ylab = "suboptimality", xlab = "nevals",
     main = "Korrelation n_eval und suboptimality", log = "xy")
# Wir sehen hier 2 verschiedene Strukturen: 1. Bei einer sehr kleinen Anzahl
# f.evals ist die 'suboptimality' recht hoch. 2. Bei einer nicht mehr sehr
# kleinen Anzahl ist sie fast 0, und wird mit steigendem 'n_eval' wieder
# etwas groesser.
# Ein ziemlich komisches Bild. Faerben wir es mal nach Algorithmen ein.

plot(data$n_eval, data$suboptimality + 1e-8,
     ylab = "suboptimality", xlab = "nevals",
     main = "Korrelation n_eval und suboptimality", log = "xy",
     col = data$algorithm)
legend("topright", legend = unique(data$algorithm), pch = 1,
       col = unique(data$algorithm))
# Newton hat eine 'suboptimality' und 0 und springt ein wenig bei
# der 'n_eval' Anzahl. BFGS streut bei 'suboptimality' ein wenig unabhaengig
# von der 'n_eval' Anzahl. Und bei GD scheint es tatsaechlich eine Korrelation
# zu geben.

plot(data$n_eval, data$distance_to_optimum, ylab = "suboptimality",
     xlab = "nevals", main = "Korrelation n_eval und suboptimality")
# Wir sehen hier auch wieder verschiedene Strukturen, sehen aber auch erstmal
# nicht viel. Darum mal wieder logarithmieren. Weil wir von oben gelernt
# haben, faerben wir mal sofort nach Algorithmen ein.

plot(data$n_eval, data$distance_to_optimum,
     ylab = "suboptimality", xlab = "nevals",
     main = "Korrelation n_eval und suboptimality", log = "xy",
     col = data$algorithm)
legend("bottomright", legend = unique(data$algorithm), pch = 1,
       col = unique(data$algorithm))
# Wir sehen wieder die deutlichen Unterschiede zwischen den 3 Algos. Und nur
# GD scheint eine Korrelation vorzuweisen.

# Giessen wir das ganze mal noch in ein paar Zahlen:

getCors <- function(d, method) {
  na.inds <- is.na(d$suboptimality)
  cor1 <- cor(d$n_eval[!na.inds], d$distance_to_optimum[!na.inds],
              method = method)
  cor2 <- cor(d$n_eval[!na.inds], d$suboptimality[!na.inds], method = method)
  return(c(dist = cor1, sub = cor2))
}
getCors(data, method = "pearson")
getCors(newton, method = "pearson")
getCors(bfgs, method = "pearson")
getCors(gradient_descent, method = "pearson")

# Interessantes Bild: Nach Pearson gibt es auf den gesamten Daten nur bei
# 'distance_to_optimum' eine Korrelation. Fuer Newton und BFGS finden sich
# jeweils kleine Werte, die auf keine Korrelation schliessen lassen. Und bei
# Gradient Descent eine grosse Korrelation.

getCors(data, method = "spearman")
getCors(newton, method = "spearman")
getCors(bfgs, method = "spearman")
getCors(gradient_descent, method = "spearman")

# Der (robustere) Spearman-Koeffizient zeigt nahezu die gleichen Ergebnisse
# Er erkennt auf dem gesamten Datensatz aber auch bei suboptimality die
# Korrelation, das hat pearson nicht geschafft. Bei den vielen Ausreissern
# die wir hier haben ist spearman auch sicherlich sinnvoller.


## 1. Problem - Wie bestimmt man beta*?
# beta* wird benoetigt, um die einzelnen Startwerte der Optimierungen zu
# bestimmen und fuer die Bestimmung der Messwerte 'suboptimality' und
# 'distance_to_opt'. beta* ist aber unbekannt, es ist ja die wahre Loesung.
# Wuerden wir die kennen, braeuchten wir keine Optimierung durchzufuehren.
# Idee hier ist es, beta* durch eine "besonders gute" Optimierung am Anfang
# vorzubestimmen. Heisst: Wir stellen die Abbruchkriterien der Optimierung
# extra streng ein. Damit stellen wir sicher, dass wir eine Loesung benutzen,
# die sehr nah am wahren beta* liegt.

## 2. Problem - lineare Trennbarkeit vermeiden
# Falls die Daten linear trennbar sind, gibt es kein eindeutig definiertes
# Optimum mehr - heisst alle Vergleiche bzgl. Optimalitaet sind hinfaellig.
# Darum muessen wir hier sicherstellen, dass das Optimum eindeutig und somit
# die Daten nicht linear trennbar sind. Die Idee ist es, zu jeder der beiden
# Klassen jeweils eine Beobachtung hinzuzufuegen, die im Zentrum der anderen
# Klasse liegt.



## Aufgabe 2:

# mySimplex - Simplex-Verfahren
# Eingabe:
#    f           - Funktion, deren Optimum(Minimum) gefunden werden soll, soll
#                  aus dem R^K auf R abbilden
#    theta.start - numeric(k), Position des Start-Simplex
#    t           - numerisch, Groesse des Simplex
#    maxit       - maximale Anzahl an Iterationen (zweites Abbruchkriterium),
#                  natuerliche Zahl, default 1000
#
# Ausgabe:
#  Liste mit 2 benannten Elementen:
#    pars - numerische Matix, ausgewertete Parameter, einer je Zeile
#    vals - Funktionswerte an den Stelle 'par'

mySimplex <- function(f, theta.start, t = 1, maxit = 1000) {
  K <- length(theta.start)
  # Als erstes brauchen wir den Startsimplex:
  d1 <- t / K / sqrt(2) * (sqrt(K + 1) + K - 1)
  d2 <- t / K / sqrt(2) * (sqrt(K + 1) - 1)
  theta <- matrix(d2, ncol = K, nrow = K)
  diag(theta) <- d1
  # Achtung: Das + theta.start funktioniert nur, weil in jeder Spalte von
  # theta ein Punkt steht, und Matrizen spaltenweise aufgebaut sind.
  theta <- cbind(0, theta) + theta.start
  
  # Jetzt brauchen wir fuer jeden Punkt in theta den Funktionswert
  theta.vals <- apply(theta, 2, f)
  
  for (i in seq_len(maxit)) {
    # Bestimme neues Theta durch Spiegelung
    h <- which.max(theta.vals)
    theta.centroid <- rowMeans(theta[, -h])
    theta.new <- 2 * theta.centroid - theta[, h]
    # Simplex updaten
    theta[, h] <- theta.new
    theta.vals[h] <- f(theta.new)
  }
  best <- which.min(theta.vals)
  return(list(pars = theta[, best], vals = theta.vals[best]))
}

mySimplex(function(x) sum(x^2), rep(10, 2))

testSimplex <- function(t = 1, tol = 5e-2) {
  makeTest <- function(rho, init) {
    fun <- function(x) 
      -dmvnorm(x, sigma = rbind(c(1, sqrt(2) * rho), c(sqrt(2) * rho, 2)))
    res <- mySimplex(fun, init, t = t)
    ## Wir finden nur eine ziemlich grobe Annaeherung an das Optimum
    expect_equal(res$par, c(0, 0), tolerance = tol)
  }
  makeTest(0.2, c(2, 2))
  makeTest(0.4, c(2, 2))
  makeTest(0.8, c(2, 2))
  makeTest(0.2, c(-1, 0.5))
}

testSimplex(t = 0.017)
testSimplex(t = 0.01)


## Aufgabe 3:
set.seed(8)
theta.star <- c(0.5, 2)
n <- 40
X <- matrix(runif(2 * n), ncol = 2)
y <- theta.star[1] + theta.star[2] * X[, 1] + theta.star[2]^2 * X[, 2] + rnorm(n)
# save(X, y, file = "daten_blatt11.RData")

targetFun <- function(theta) {
  m <- theta[1] + theta[2] * X[, 1] + theta[2]^2 * X[, 2]
  drop(crossprod(y - m))
}


makeMap <- function(method, grid.left, grid.right) {
  # Run the optimization over the grid
  x2 <- seq(grid.left, grid.right, by = 0.1)
  x2.grid <- expand.grid(x2, x2)
  opt.res <- apply(x2.grid, 1, function(z)
    optim(z, targetFun, method = method)$par)
  # Decide, which run converged to which optimum
  opt.res <- round(opt.res, 1)
  opt <- factor(apply(opt.res, 2, function(x) 100 * x[1] + x[2]))
  
  # Make the first part of the plot
  plot(x2.grid, col = c("lightgreen", "turquoise")[opt], pch = 15, main = method,
       xlab = expression(theta[1]), ylab = expression(theta[2]))
  
  # Mark the true optima
  optima <- apply(opt.res, 1, function(x) unique(x))
  #names(optima) <- c("x", "y")
  points(optima, col = c("blue", "darkgreen"), pch = 16)
  
  # Add the contour lines
  x <- seq(grid.left, grid.right, by = 0.05)
  x.grid <- expand.grid(x, x)
  y <- matrix(apply(x.grid, 1, targetFun), nrow = length(x))
  contour(x, x, y,add = TRUE, labels = NULL,
          levels = c(44, 48, 54, 57, 70, 100, 150, 300, 500, 700, 1000, 2000))
}

makeMap("BFGS", grid.left = -3, grid.right = 4)
makeMap("Nelder-Mead", grid.left = -3, grid.right = 4)
