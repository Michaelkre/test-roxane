library(testthat)
source("lcf.R")
source("var.R")

## Aufgabe 1

## a)

## Der Rekursionanfang bei Vektoren x der Laenge 1
## T = x (Summe ueber den Vektor entspricht dem einzigen Element)
## S = 0 (Eine einzelne Beobachtung weicht nicht von sich selbst ab)

## Jetzt teilen wir den Algorithmus auf. Dazu definieren wir
## m = floor(length(x) / 2) und n = length(x) - m
## Dann haben wir zunaechst fuer T recht einfach die Summe aus dem T der
## 1. Haelfte von x und dem T aus der 2. Haelfte von x
## T[1:(m + n)] = T[1:m] + T[(m + 1):(m + n)]
## S ist ein wenig komplizierter, die entsprechende Formel wurde im Skript
## bewiesen:
## ## S[1:(m + n)] = S[1:m] + S[(m + 1):(m + n)] +
##                  m / (n * (m + n)) * (n / m * T[1:n] - T[(m + 1):(m + n)])^2


## b)

## Helferfunktion fuer den pairwise Varianz Algorithmus. Rekursiver Algorithmus,
## der die eigentliche Arbeit macht. Um den komplizierten Output (Liste mit
## den beiden Elementen T und S) zu verstecken, wird der untere Wrapper
## genutzt.

varRec <- function(x) {
  ## Rekursionsanfang:
  ## T entspricht der Summe des Vektors, ist also hier x
  ## S entspricht der Quadratsumme, muss also hier 0 sein
  if (length(x) == 1)
    return(list(T = x, S = 0))
  
  ## Rekursion mit den beiden halben Vektoren.
  m <- floor(length(x) / 2)
  n <- length(x) - m
  left <- Recall(x[1:m])
  right <- Recall(x[(m + 1):(m + n)])
  
  ## Jetzt das finale Ergebnis nach den Rekursionsformeln (32) und (33)
  TT <- left$T + right$T
  S <- left$S + right$S + m / (n * (m + n)) * (n / m * left$T - right$T)^2
  return(list(T = TT, S = S))
}

## pairwiseVar - Varianzberechnung nach der rekursiven Formel im Skript
##
## Input:
##   x - numerischer Vektor
##
## Output:
##   numerischer Wert - die bestimmte Varianz. 

pairwiseVar <- function(x) {
  varRec(x)$S / (length(x) - 1)
}


## Aufgabe 2

## a)

# Dieser kleine Helfer erzeugt uns die Shifted-Varianz Algorithmen:
#
# Input:
#   Eine Funktion, die einen Algorithmus zur Varianzberechnung implementiert.
#
# Output:
#   Eine Funktion, die den selben Algorithmus benutzt, allerdings vorher die
#   Eingabe um den Mittelwert der ersten m Elemente verschiebt.
#   Das Interface der neuen Funktionen entspricht dem der alten.

makeShiftedVar <- function(varAlgo, m) {
  if (m == 0) {
    varAlgo
  } else {
    function(x) varAlgo(x - mean(x[1:m]))
  }
}

## b)

shiftedBenchmarkOne <- function(varFun, add) {
  n <- 1000
  vars <- sapply(0:10, function(m) makeShiftedVar(varFun, m)((add + 1:n) / 100))
  difs <- vars - n * (n + 1) / 12 / 100^2
  return(difs)
}

shiftedBenchmark <- function(varFuns, add = 10^6){
  res <- sapply(varFuns, shiftedBenchmarkOne, add = add)
  names <- c("textbookVar", "twoPassVar", "corTwoPassVar", "youngsCramerVar",
             "pairwiseVar")
  par(mfrow=c(2,2))
  for (i in 1:4) {
    plot(0:10, abs(res[,i]), log="y", ylim=range(abs(res)), xlab = "m", 
         ylab = expression(log[2](group("|",Differenz,"|"))), main = names[i], 
         pch = 16)
  }
  par(mfrow = c(1, 1))
  return(res)
}
varFuns <- c(textbookVar, twoPassVar, corTwoPassVar, youngsCramerVar,
             pairwiseVar)

# Fuer (10^6 + 1:1000) / 100
shiftedBenchmark(varFuns)
# Verschiebung korrigiert den groesseren Fehler bei textbook und youngsCramer. 
# twoPass  und corTwoPass sind unabhaengig von m konstant gut mit einem Fehler
# von ca. 1e-14. 
# Fehler kommen hier primaer durch Fortpflanzung der Rundungsfehler in der 
# Eingabe zustande.

# Fuer (10^16 + 1:1000) / 100
shiftedBenchmark(varFuns, add = 10^16)
# Auch hier korrigiert die Verschiebung den grossen Fehler bei textbook und 
# youngsCramer. Im Gegensatz zu oben sehen wir hier auch fuer twoPass noch eine
# kleiner Verbesserung durch die Verschiebung. corTwoPass hingegen ist weiterhin
# konstant gut mit einem Fehler in der Groessenordnung von 1e-5.
# Hier kommt zusaetzlich zu der Fortpflanzung der Rundungsfehler aus der Eingabe
# auch noch hinzu, dass die Eingabewerte so gross sind, dass Summen darueber 
# schnell nicht mehr darstellbar sind. Letzteres Problem wird durch die Ver-
# schiebung bereits behoben.

## c)

# Es bringt zwar nichts, aber: Wenn wir m gegen n gehen lassen, geht der Wert
# des shifts gegen wahren Mittelwert. Fuer m = n ziehen wir den tatsaechlichen
# Mittelwert ab und der Mittelwert der verschobenen Daten ist 0. Heisst:
# Wir brauchen den Mittelwert im Two Pass selbst gar nicht zu bestimmen!
# Sondern koennen einfach die Quadratsumme der Beobachtungen nehmen. Nachteil:
# Fuer m = n muessen wir einmal komplett durch die Daten, den Durchlauf den
# wir uns sparen machen wir also vorher schon.

# Das ist jetzt einfach: m = 1 scheint ausreichend. Dies liegt aber natuerlich
# an der Datensituation, wo die Verschiebung um einen beliebigen Wert uns 
# nah an eine Zentrierung bringt. Bei weniger kuenstlichen Daten sollte ein
# m gewaehlt werden, sodass eine verlaessliche Schaetzung des Mittelwertes
# moeglich ist. Einen festen Wert fuer m gibt es nicht.



## Aufgabe 3
# Volle Periode, denn jeweils
698769069 %% 8 == 5
5 %% 8 == 5
# ist 5 und c ist ungerade. Also sollten beide Generatoren volle
# Periodenlaenge haben

zzs1 <- lcf(n = 1e5, seed = 27, a = 698769069, ce = 453816693, m = 2^31)
zzs2 <- lcf(n = 1e5, seed = 27, a = 5, ce = 453816693, m = 2^31)
anyDuplicated(zzs1)
anyDuplicated(zzs2)

# Offensichtlich hat der erste Generator aber eine wesentlich kuerzere
# Periode. Warum?

# Problem sind Rundungsfehler bei der Operation a * x + c aufgrund der Groesse 
# der Werte.
# Falls hier gerundet wird (ist bei a = 698769069 der Fall), nimmt der RNG
# nicht mehr die volle Periode an.
# Genaue Erklaerung folgt in der Vorlesung.