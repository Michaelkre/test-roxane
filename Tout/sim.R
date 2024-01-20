# Die bekannten Funktionen zur Konstruktion von Zielke Matrizen

## zielkeMatrix berechnet die Zielke-Matrix vom Typ X_Z1(Z,n).
## Eingabe:
# Z: quadratische pxp-Matrix oder Skalar
# m: numerische Zahl, Anzahl Blockelemente pro Zeile / Spalte
## Ausgabe:
# Inverse der Zielke-Matrix

zielkeMatrix = function(Z, m){
  
  # Param Checks: Z muss eine Matrix sein, notfall 1 x 1
  Z = as.matrix(Z)
  stopifnot(is.matrix(Z), is.numeric(Z),
            nrow(Z) == ncol(Z), m >= 3)
  
  # Hilfsmatrizen
  p = nrow(Z)
  Ip = diag(p)
  Einsen = matrix(1, nrow = m, ncol = m)
  
  # Ergebnismatrix mit Kronecker Produkt initialisieren
  # In jedem Block steht erstmal Z, und wir muessen jetzt schauen, was wir
  # noch veraendert muessen
  X = Einsen %x% Z
  
  # 1. Zeilenblock - wir muessen passende Einheitsmatrizen addieren
  blockmult = c(1:(m - 1), 0)
  X[1:p, ] =  X[1:p, ] + t(blockmult) %x% Ip
  
  # Iteration ueber die uebrigen Spalten, oben nach unten
  # muss immer auf einen Spaltenblock weniger Ip addiert werden
  for (i in 2:(m - 1)) {
    blockmult[m - i + 1] = blockmult[m - i + 1] - 1
    blockinds = ((i - 1) * p + 1):(i * p)
    X[blockinds, ] = X[blockinds, ] + (t(blockmult) %x% Ip)
  }
  
  # Letzter Zeilenblock
  blockmult = c(0:(m - 2), -1)
  blockinds = ((m - 1) * p + 1):(m * p)
  X[blockinds, ] =  X[blockinds, ] + (t(blockmult) %x% Ip)
  return(X)
}

## invZielkeMatrix berechnet die Inverse der Zielke-Matrix vom Typ X_Z1(Z, n).
## Eingabe:
# Z: quadratische pxp-Matrix oder Skalar
# m: numerische Zahl, Anzahl Blockelemente pro Zeile / Spalte
## Ausgabe:
# Inverse der Zielke-Matrix

invZielkeMatrix <- function(Z, m){
  # Param Checks: Z muss eine Matrix sein, notfall 1 x 1
  Z = as.matrix(Z)
  stopifnot(is.matrix(Z), is.numeric(Z),
            nrow(Z) == ncol(Z), m >= 3)
  
  # Hilfsmatrizen
  p = nrow(Z)
  n = m * p
  Ip = diag(p)
  
  # Ergebnis initialisieren
  invX = matrix(0, nrow =n, ncol = n)
  
  # 1. Spalte
  colmult.Z = c(-1, rep(0, m - 2), 1)
  colmult.I = c(-(m - 2), rep(0, m - 3), 1, (m - 2))
  invX[, 1:p] = invX[, 1:p] + colmult.Z %x% Z + colmult.I %x% Ip
  
  # Iteration durch die Spalten
  colmult.1 = c(1, rep(0, m - 2), -1)
  for (i in 2:(m - 1)) {
    colmult.2 = c(rep(0, m - 1 - i), 1, -1, rep(0, i - 1))
    colind = ((i  -1) * p + 1):(i * p)
    invX[, colind] = invX[, colind] + (colmult.1 + colmult.2) %x% Ip 
  }
  
  # Letzte Spalte
  colind = (n-p+1):n
  invX[1:p, colind] = Z
  invX[(n - p + 1):n, colind] = -Z - Ip
  
  return(invX)
}

# wMatrix - Erzeugt eine Wichtungsmatrix, mit der die exakte Inverse
#           eine Rangreduzierten Zielkematrix bestimmt werden kann
# Input
#   n - Ganze Zahl, Groesse der Matrix. Muss gerade und groesser als 4 sein.
#
# Output
#   Eine numerische Matrix, die gewuenschte Wichtungsmatrix
wMatrix <- function(n) {
  stopifnot(as.integer(n) == n, n >= 4, n %% 2 == 0)
  
  rotate_vector <- function(x, by) {
    s <- length(x) - by + 1
    c(x[s:length(x)], x[1:(s - 1)])
  }
  
  l <- (n - 2) / 2
  m <- 2 * l + 2
  ms <- l / m
  
  ## Constants:
  a <- 1 / m
  b <- 1 - ms
  c <- ms
  ## Construct matrix:
  W <- matrix(0, n, n)
  W[, 1] <- c( b, -a,  a, c(-1,  1) * rep(a, n - 4),  c)
  W[, 2] <- c(-a,  b,  c, c( 1, -1) * rep(a, n - 4),  a)
  W[, 3] <- c( a,  c,  b, c(-1,  1) * rep(a, n - 4), -a)
  W[, n] <- c( c,  a, -a, c( 1, -1) * rep(a, n - 4),  b)
  if (n > 4)
    for (i in 4:(n-1))
      W[, i] <- rotate_vector(W[, i - 2], 2)
  W
}

# leftHandSide - erzeugt 4 linke Seiten fuer eine gegebene Designmatrix
# 
# Input
#   X - numerische Matrix, Designmatrix
#   Xinv - numerische Matrix, Inverse zu X
#
# Output
#   Benannte Liste mit den Elementen
#   b - numerischer Matrix mit 4 Spatlen,
#       wahre Koeffizientenvektoren des KQ Problems
#   y - numerische Matrix mit 4 linken Seiten, eine pro Spalte
#   phi - numerischer Vektor, Winkel der 4 linken Seiten
leftHandSide <- function(X, Xinv) {
  norm <- function(x) sqrt(sum(x*x))
  
  y0 <- trunc(rnorm(ncol(Xinv), sd = 256))
  beta0 <- drop(Xinv %*% y0)
  
  y <- drop(X %*% beta0)
  norm_y <- norm(y)
  
  r0 <- y -y0
  norm_r0 <- norm(r0)
  
  i <- 19L
  while (norm_r0 < (norm_y * 2^i))
    i <- i - 2L
  if (i < -19L)
    i <- -21L
  ck <- 2^(c(-21L, -3L, 1L, 19L) - i)
  y <- y + r0 %*% t(ck)
  list(beta = Xinv %*% y, y = y, phi = atan(ck))
}

## Jetzt die verschiedenen Algorithmen fuer KQ
# MGS und greville sind die bekannten Musterloesungen

# greville - Berechnet die G-Inverse mittels des Greville Verfahrens
#
# Eingabe:
#
# Ausgabe:

##hv: Entfernt, das habt ihr euch wohl so gedacht!


# mgs - Modified Gram-Schmidt Orthogonalisierung
#
# Eingabe:
#  X - Matrix
#
# Ausgabe:
#  Generalisierte Inverse von X

mgs <- function(X) {
  choosePivotColumn <- function(X, cols) {
    XX <- X[, cols]
    cols[which.max(colSums(XX * XX))]
  }
  exchangeColumns <- function(X, i, j, which = 1:nrow(X)) {
    if (i != j) {
      tmp <- X[which, i]
      X[which, i] <- X[which, j]
      X[which, j] <- tmp
    }
    X
  }
  # Ersetze breite durch hohe Matrix. (Immer voller Spaltenrang)
  if (nrow(X) < ncol(X))
    return(t(Recall(t(X))))
  
  # Initialisiere alle benoetigten Variablen.
  k <- ncol(X)
  d <- numeric(k)
  U <- diag(1, nrow = k)
  P <- U
  
  # Nur orthogonalisieren, wenn es mehr als eine Spalte gibt!
  if (k > 1) {
    for (s in 1:(k-1)) {
      # Hier muss das Pivot gewaehlt werden:
      pivot <- choosePivotColumn(X, s:k)
      # Jetzt tauschen wir in allen Matrizen das Pivot nach vorne
      X <- exchangeColumns(X, s, pivot)
      P <- exchangeColumns(P, s, pivot)
      if (s > 1)
        U <- exchangeColumns(U, s, pivot, 1:(s-1))
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
  # Und hier muss vorne P anmultipliziert werden
  X <- P %*% backsolve(U, diag(1 / d, nrow = length(d)) %*% t(X))
  return(X)
}

# lsSolveXXX - loest ein KQ-Problem mit dem jeweiligem Verfahren
#
# Input
#  X - numerische Matrix, Designmatrix
#  y - numerischer Vektor, linke Seite. Kann auch eine numerische
#      Matrix sein, dann werden mehrere linke Seiten gleichzeitig
#      geloest, eine linke Seite entspricht einer Spalte.
#
# Output
#  numerischer Vektor, Loesung des KQ Problems
lsSolveNormal <- function(X, y) {
  At <- t(X)
  AtA <- At %*% X
  Atb <- At %*% y
  drop(solve(AtA, Atb, tol = 1e-30))
}

lsSolveLM <- function(X, y) {
  lm.fit(X, y)$coefficients
}

lsSolveMGS <- function(X, y) {
  QR <- mgs(X)
  drop(QR %*% y)
}

lsSolveGreville <- function(X, y) {
  QR <- greville(X)
  drop(QR %*% y)
}