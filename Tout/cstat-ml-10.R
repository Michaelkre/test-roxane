library(numDeriv)

## Aufgabe 1:

mOptim <- function(init, f, eps = 1e-12, maxit = 200, method) {
  
  stopifnot(
    is.numeric(init),
    is.function(f),
    is.numeric(f(init)),
    length(f(init)) == 1L,
    is.numeric(eps),
    length(eps) == 1L,
    eps > 0,
    as.integer(maxit) == maxit,
    length(maxit) == 1L,
    maxit > length(init),
    method %in% c("QN", "N", "K", "G")
  )
  
  ## Initialisierung
  n <- length(init)
  k <- 1
  es <- 1:n
  
  ## Matrix, um alle betas zu speichern
  betas <- matrix(0, nrow = n, ncol = maxit + 1)
  betas[, 1] <- init
  
  repeat {
    ## Ueberpruefung auf Abbruch nur moeglich, wenn k > n wegen Koord-Abstieg.
    if (k > n) {
      if(method != "K"){
        if (k >= maxit || sqrt(sum((betas[, k] - betas[, (k - 1)])^2)) < eps)
          break
      }
      if(method == "K"){
        if(k >= maxit || sqrt(sum((betas[, k] - betas[, (k - n)])^2)) < eps)
          break
      }
    }
    ## Verschiedene Abbruchkriterien fuer Koordinatenabstieg, da hier ein
    ## quasi-0 Update in einer einzelnen Achse durchaus vorkommen kann und nicht
    ## zum Abbruch fuhren sollte. Erst, wenn ein ganzer Lauf ueber alle n
    ## Achsen kein Update gebracht hat, soll abgebrochen werden.
    
    if(method == "K"){
      ## Waehle Einheitsvektor fuer diese Iteration anhand von 'es', entferne
      ## das 1. Element aus 'es'
      e <- rep(0, n)
      e[es[1]] <- 1
      es <- c(es[-1], es[1])
      Richtung <- e
    }
    
    if(method == "G"){
      D <- - 1
    }
    
    if(method == "N"){
      D <- - solve(hessian(f, betas[, k]))
    }
    
    if(method == "QN"){
      ## falls es noch kein P gibt, waehle Start:
      if(k == 1)
        P <- diag(1, nrow = length(betas[, k]))
      ## falls P schon existiert, muss P ein Update bekommen
      if(k > 1){
        ## Wir brauchen:
        Dbeta <- betas[, k] - betas[, k - 1]
        g <- grad(f, betas[, k]) - grad(f, betas[, k - 1])
        
        ## DFP Update
        U1 <- (Dbeta %*% t(Dbeta)) / drop((t(g) %*% Dbeta))
        U2Z <- P %*% g %*% t(g) %*% P 
        U2N <- t(g) %*% P %*% g
        U2 <- U2Z / drop(U2N)
        U <- U1 - U2
        P <- P + U
      }
      D <- - P
    }
    
    ## LineSearch
    if(method != "K") 
      Richtung <- D %*% grad(f, betas[, k])
    
    fLine <- function(nu) f(betas[, k] + nu * Richtung)
    ## Rufe jetzt optimize zur lineSearch auf und waehle den naechsten Suchpunkt
    nu <- optimize(f = fLine, interval = c(-1, 1))$minimum
    ## Die Koordinatensuche hat keinen Gradienten, an dem sie die Richtung 
    ## ablesen koennte. Daher muss in beide Richtungen geprueft werden.
    
    betas[, k + 1] <- betas[, k] + nu * Richtung
    k <- k + 1
  }
  ## Achtung: k ist immer 1 groesser als die Iterationszahl hier.
  return(par = betas[, k])
}

f <- function(x) (x[1]-2)^2 + (x[2] + 1)^2 + x[3]^2

opts <- c("K", "G", "QN", "N")
sapply(opts, function(m) mOptim(rep(0, 3), f, method = m))

