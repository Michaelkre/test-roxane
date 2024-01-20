
lcs <- function(n, seed, a = 1844544229, c = 453816693, m = 2^31) {
  res <- numeric(n)
  x <- seed
  for (i in 1:n) {
    ## einzelner Schritt der erzeugenden Rekursion
    x <- (a * x + c) %% m
    ## speichere das Ergebnis
    res[i] <- x
  }
  return(res)
}

uniformDistrTest <- function(u, k, D) {
  n <- length(u)
  ## Wir muessen die letzten Elemente wegwerfen, jedes Tupel muss
  ## vollstaendg sein 
  y <- floor(u[1:(n - n %% k)] * D)
  # Bilde die Tupel und verwende table um an die Haeufigkeiten zu kommen
  tuples <- lapply(1:k, function(i) factor(y[seq(i, to = length(y), by = k)],  levels = 0:(D-1)))
  freq <- as.vector(table(tuples))
  # Sicherheitscheck - sollte immer klappen:
  if (length(freq) != D^k)
    stop(paste("Something went wrong", "D=", D, "und", "k=", k))
  ## wende chisq.test an - die default Werte unterstuetzen, was wir wollen
  ## (erwartete Haeufigkeiten)
  return(chisq.test(freq)$p.value)
}
