library(testthat)

## Aufgabe 1


### a)

## zielkeMatrix berechnet die Zielke-Matrix vom Typ X_Z1(Z,n).
## Eingabe:
# Z: quadratische pxp-Matrix oder Skalar
# m: numerische Zahl, Anzahl Blockelemente pro Zeile / Spalte
## Ausgabe:
# Inverse der Zielke-Matrix

zielkeMatrix <- function(Z, n){
  
  # Param Checks: Z muss eine Matrix sein, notfall 1 x 1
  Z <- as.matrix(Z)

  # Hilfsmatrizen
  p <- nrow(Z)
  m <- n / p
  Ip <- diag(p)
  Einsen <- matrix(1, nrow = m, ncol = m)
  
  stopifnot(is.matrix(Z), is.numeric(Z),
            nrow(Z) == ncol(Z), m >= 3)
  
  # Ergebnismatrix mit Kronecker Produkt initialisieren
  # In jedem Block steht erstmal Z, und wir muessen jetzt schauen, was wir
  # noch veraendert muessen
  X <- Einsen %x% Z
  
  # 1. Zeilenblock - wir muessen passende Einheitsmatrizen addieren
  blockmult <- c(1:(m - 1), 0)
  X[1:p, ] <- X[1:p, ] + t(blockmult) %x% Ip
  
  # Iteration ueber die uebrigen Spalten, oben nach unten
  # muss immer auf einen Spaltenblock weniger Ip addiert werden
  for (i in 2:(m - 1)) {
    blockmult[m - i + 1] <- blockmult[m - i + 1] - 1
    blockinds <- ((i - 1) * p + 1):(i * p)
    X[blockinds, ] <- X[blockinds, ] + (t(blockmult) %x% Ip)
  }
  
  # Letzter Zeilenblock
  blockmult <- c(0:(m - 2), -1)
  blockinds <- ((m - 1) * p + 1):(m * p)
  X[blockinds, ] <- X[blockinds, ] + (t(blockmult) %x% Ip)
  return(X)
}


testZielke <- function(){
  test_that("Beispiel 2.2", {
    X <- matrix(c(
      999, 1000, 998,
      999, 999, 998,
      998, 999, 997), nrow = 3, byrow = TRUE)
    expect_equal(zielkeMatrix(998, 3), X)
  })
  test_that("Beispiel 2.3", {
    X <- matrix(c(
      999, 1000, 1001, 998,
      999, 1000, 1000, 998,
      999, 999, 1000, 998,
      998, 999, 1000, 997), nrow = 4, byrow = TRUE)
    expect_equal(zielkeMatrix(998, 4), X)
  })
}

testZielke()

## b)

## invZielkeMatrix berechnet die Inverse der Zielke-Matrix vom Typ X_Z1(Z, n).
## Eingabe:
# Z: quadratische pxp-Matrix oder Skalar
# m: numerische Zahl, Anzahl Blockelemente pro Zeile / Spalte
## Ausgabe:
# Inverse der Zielke-Matrix

invZielkeMatrix <- function(Z, n){
  # Param Checks: Z muss eine Matrix sein, notfall 1 x 1
  Z <- as.matrix(Z)

  # Hilfsmatrizen
  p <- nrow(Z)
  m <- n / p
  Ip <- diag(p)
  
  stopifnot(is.matrix(Z), is.numeric(Z),
            nrow(Z) == ncol(Z), m >= 3)
  
  # Ergebnis initialisieren
  invX <- matrix(0, nrow =n, ncol = n)
  
  # 1. Spalte
  colmult.Z <- c(-1, rep(0, m - 2), 1)
  colmult.I <- c(-(m - 2), rep(0, m - 3), 1, (m - 2))
  invX[, 1:p] <- invX[, 1:p] + colmult.Z %x% Z + colmult.I %x% Ip
  
  # Iteration durch die Spalten
  colmult.1 <- c(1, rep(0, m - 2), -1)
  for (i in 2:(m - 1)) {
    colmult.2 <- c(rep(0, m - 1 - i), 1, -1, rep(0, i - 1))
    colind <- ((i  -1) * p + 1):(i * p)
    invX[, colind] <- invX[, colind] + (colmult.1 + colmult.2) %x% Ip 
  }
  
  # Letzte Spalte
  colind <- (n-p+1):n
  invX[1:p, colind] <- Z
  invX[(n - p + 1):n, colind] <- -Z - Ip
  
  return(invX)
}


testInvZielke <- function(){
  test_that("Beispiel 2.2", {
    X <- matrix(c(
      -999, 2, 998,
      1, -1, 0,
      999, -1, -999), nrow = 3, byrow = TRUE)
    expect_equal(invZielkeMatrix(998, 3), X)
  })
  test_that("Beispiel 2.3", {
    X <- matrix(c(
      -1000, 1, 2, 998,
      0, 1, -1, 0,
      1, -1, 0, 0,
      1000, -1, -1, -999), nrow = 4, byrow = TRUE)
    expect_equal(invZielkeMatrix(998, 4), X)
  })
}

testInvZielke()


testZielkeSonstige <- function(){
  test_that("5x5", {
    expect_equal(zielkeMatrix(100, 5) %*% invZielkeMatrix(100, 5), diag(5))
  })
  test_that("8x8", {
    expect_equal(zielkeMatrix(998, 8) %*% invZielkeMatrix(998, 8), diag(8))
  })
  test_that("Z Matrix 1", {
    expect_equal(zielkeMatrix(diag(2), 8) %*%
                   invZielkeMatrix(diag(2), 8), diag(8))
  })
  test_that("Z Matrix 2", {
    X = zielkeMatrix(matrix(c(24,24,245,678), nrow = 2), 8) %*%
      invZielkeMatrix(matrix(c(24,24,245,678), nrow = 2), 8)
    expect_equal(X, diag(8))
  })
}

testZielkeSonstige()

## (Kuerzere) Alternative ohne Schleifen: 
# a)
zielkeMatrix <- function(Z, n) {
  Z <- as.matrix(Z)
  p <- ncol(Z)
  m <- n / p
  stopifnot(is.matrix(Z), is.numeric(Z),
            nrow(Z) == ncol(Z), m >= 3)
  Z1 <- matrix(1, m, m) %x% Z + 
    (matrix(c(1:(m - 1), 1), nrow = m, ncol = m, byrow = TRUE) - 
       lower.tri(matrix(1, m, m), diag = TRUE)[, m:1] - 
       matrix(c(rep(0, m^2 - 1), 1), ncol = m, nrow = m)) %x% diag(p)
  return(Z1)
}

testZielke()

# b)
invZielkeMatrix <- function(Z, n) {
  Z <- as.matrix(Z)
  p <- ncol(Z)
  m <- n / p
  stopifnot(m >= 3, m %% 1 == 0)
  
  einheits.mult <- -diag(m)[, m:1] 
  einheits.mult[, 1:(m - 1)] <- einheits.mult[, 1:(m - 1)] - einheits.mult[, 2:m]
  einheits.mult[1, ] <- c(-(m - 2), rep(1, m - 3), 2, 0)
  einheits.mult[m, ] <- c(m - 2, rep(-1, m-1))
  
  z.mult <- matrix(0, nrow = m, ncol = m)
  z.mult[1, c(1, m)] <- c(-1, 1)
  z.mult[m, ] <- -z.mult[1, ]
  
  return(z.mult %x% Z + einheits.mult %x% diag(p))
}
testInvZielke()
testZielkeSonstige()

# Aufgabe 3 - MGS

# Wir fangen an mit 2 kleinen Helfer-Funktionen fuer die Spaltenpivotierung:

# choose_pivot_column - waehle Pivotspalte aus
#
# Eingabe:
#  X - numerische Matrix
#  cols - Spalten aus denen das Pivot gewaehlt wird
#
# Ausgabe:
#  Index der Pivotspalte
choose_pivot_column <- function(X, cols) {
  XX <- X[, cols]
  cols[which.max(colSums(XX^2))]
}

# exchange_columns - vertausche zwei Spalten einer Matrix
#
# Eingabe:
#  X - Matrix
#  i, j - Indizes der Spalten die vertauscht werden
#  which - Welche Zeilen vertauscht werden
#
# Ausgabe:
#  Permutierte Matrix
exchange_columns <- function(X, i, j, which = 1:nrow(X)) {
  if (i != j) {
    tmp <- X[which, i]
    X[which, i] <- X[which, j]
    X[which, j] <- tmp
  }
  X
}

# mgs - Modified Gram-Schmidt orthogonalization
#
# Eingabe:
#  X - numerische Matrix mit vollem Spalten oder Zeilenrang
#
# Ausgabe:
#  Generalisierte Inverse von X

mgs <- function(X) {
  ## Ersetze breite durch hohe Matrix. (Immer voller Spaltenrang)
  if (nrow(X) < ncol(X))
    return(t(Recall(t(X))))
  k <- ncol(X)
  d <-  numeric(k)
  P <- U <- diag(k)
  ## Nur orthogonalisieren, wenn es mehr als eine Spalte gibt!
  if (k > 1) {
    for (s in 1:(k - 1L)) {
      pivot <- choose_pivot_column(X, s:k)
      X <- exchange_columns(X, s, pivot)
      P <- exchange_columns(P, s, pivot)
      if (s > 1) {
        U <- exchange_columns(U, s, pivot, 1:(s - 1L))
      }
      qs <- X[, s]
      d[s] <- crossprod(qs)
      for (i in (s + 1):k) {
        qi <- X[, i]
        U[s, i] <- crossprod(qi, qs) / d[s]
        X[, i] <- qi - U[s, i] * qs
      }
    }
  }
  d[k] <- crossprod(X[, k])
  P %*% backsolve(U, diag(1 / d, nrow = k)) %*% t(X)
}

mgsTest <- function() {
  # Ein paar Tests mit zufaelligen Matrizen, darum seed setzen:
  set.seed(1273)
  for (k in c(1, 2, 4, 8, 16)) {
    X <- matrix(sample(100, 32), ncol = k)
    expect_equal(X %*% mgs(X) %*% X, X)
  }
}

mgsTest()


testMPInverse <- function(mpinverse) {
  ## Workhorse
  test_it <- function(X, tolerance = 1e-3) {
    Xp <- try(mpinverse(X), silent = TRUE)
    expect_true(is.matrix(Xp))
    if (is.matrix(Xp)) {
      expect_equal(nrow(Xp), ncol(X), scale = 1)
      expect_equal(ncol(Xp), nrow(X), scale = 1)
      if (nrow(Xp) == ncol(X) && ncol(Xp) == nrow(X)) {
        expect_equal(X %*% Xp %*% X, X, tolerance = tolerance)
        expect_equal(Xp %*% X %*% Xp, Xp, tolerance = tolerance)
        expect_equal(t(X %*% Xp), X %*% Xp, tolerance = tolerance)
        expect_equal(t(Xp %*% X), Xp %*% X, tolerance = tolerance)
      }
    }
  } 
  
  # Ein paar konstanten zum Testen:
  X1 <- matrix(runif(25), 5, 5)
  X2 <- matrix(runif(20), 5, 4)
  X3 <- matrix(runif(42), 3, 14)
  X4 <- matrix(runif(8), 1, 8)
  X5 <- matrix(runif(8), 8, 1)
  
  test_that("X1 matrix", {test_it(X1)})
  test_that("X2 matrix", {test_it(X2)})
  test_that("X3 matrix", {test_it(X3)})
  test_that("X4 matrix", {test_it(X4)})
  test_that("X5 matrix", {test_it(X5)})
  test_that("Zeilenvektor", {test_it(matrix(1:10, nrow = 1))})
  test_that("Spaltenvektor", {test_it(matrix(1:10, ncol = 1))})
}

testMPInverse(mgs)
