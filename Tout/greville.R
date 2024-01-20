# greville - Berechnet die G-Inverse mittels des Greville-Verfahrens
#
# Eingabe:
#  X - numerische Matrix mit vollem Zeilen- oder Spaltenrang
#
# Ausgabe:
#  Generalisierte Inverse von X

greville <- function(X){
  if (nrow(X) > ncol(X)) return(t(Recall(t(X))))
  
  X.plus <- as.matrix(X[1, ] / sum(X[1, ]^2))
  
  for(j in 1 + seq_len(nrow(X) - 1)){
    djt <- X[j, ] %*% X.plus
    cjt <- X[j, ] - djt %*% X[1:(j-1), ]
    bj <- t(cjt) / sum(cjt^2)
    X.plus <- cbind(X.plus - bj %*% djt, bj)
  }
  return(as.matrix(X.plus))
}