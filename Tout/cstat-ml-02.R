library(testthat)
library(microbenchmark)

set.seed(1234)

## Aufgabe 1

## a)

quickSortRek <- function(a) {
  
  if (length(a) < 2)
    return(a)
  
  smaller <- a[1] >= a
  c(Recall(a[smaller][-1]), a[1], Recall(a[!smaller]))
}


## b)

quickSortIt <- function(a) {
  # Rekursionsstacks initialisieren
  l.stack <- 1
  r.stack <- length(a)
  # Zaehler, wo auf dem Stack wir uns befinden
  i <- 0
  
  # Solange wir den Stack noch nicht abgearbeitet haben
  while (i < length(l.stack)) {
    # Stackzaehler erhoehen
    i <- i + 1
    
    # l und r vom Stack holen
    l <- l.stack[i]
    r <- r.stack[i]
    
    # Rekursionabbruch
    if (r - l < 1)
      next
    
    # Pivot ziehen, ich nehme mal immer k
    pivot <- sample(l:r, 1)
    
    # Quicksort Vertauschungen: alles kleine nach links, das grosse nach rechts
    while (l != r) {
      while (a[l] <= a[pivot] & l < pivot) {
        l <- l + 1
      }
      while (a[r] > a[pivot] & r > pivot) {
        r <- r - 1
      }
      a[c(l, r)] <- a[c(r, l)]
      if (l == pivot)
        pivot <- r
      else if (r == pivot)
        pivot <- l
    }
    
    # Rekursion: lege die naechsten Aufrufe auf den Stack
    l.stack <- c(l.stack, l.stack[i], pivot + 1)
    r.stack <- c(r.stack, pivot - 1, r.stack[i])
  }
  return(a)
}


microbenchmark(quickSortRek(sample(5000)))
microbenchmark(quickSortIt(sample(5000)))

# Rekursiv ist dann doch schneller. Weil bei der Rekursion R den ganzen
# Verwaltungsaufwand fuer uns uebernimmt, was deutlich besser ist als es
# selbst zu machen.


## Aufgabe 2:

## a) und b)

# binarySearch - Bestimmt Position, an der ein Element in einen sortierten Vektor
#                 eingefuegt wird
#
# Input
#  X - Numerischer Vektor, sortiert, in den eingefuegt werden soll
#  x - Numerischer Vektor der Laenge 1, soll eingefuegt werden
# Output
#  Benannte Liste mit den Elementen
#    result - Hinter welcher Position muss x eingefuegt werden
#    n.cmps - Anzahl benoetigter Vergleiche


binarySearch <- function(X, x) {
  # Rekursionsanfang 1: Bei leerem Vektor nach dem 0. Element einfuegen
  if(length(X) == 0L){
    return(list(result = 0L, n.cmps = 0L))
  }
  
  # Rekursionsanfang 2: Bei Laenge 1 entweder vor oder hinter dem Element einfuegen
  if(length(X) == 1L) {
    if(x < X) {
      return(list(result = 0L, n.cmps = 1L))
    } else {
      return(list(result = 1L, n.cmps = 1L)) 
    }
  }
  
  # Bestimme das mittlere Element
  m <- floor(length(X) / 2)
  
  # Entscheide ob Rekursion in linke oder rechte Haelfte
  if(x < X[m]) {
    rec.res <- binarySearch(X[seq_len(m)], x)
    return(list(
      result = rec.res$result,
      n.cmps = 1 + rec.res$n.cmps
    ))
  } else {
    rec.res <- binarySearch(X[(m + 1):length(X)], x)
    return(list(
      result = m + rec.res$result,
      n.cmps = 1 + rec.res$n.cmps
    ))
  }
}

testBinarySearch <- function() {
  for(i in c(5, 10, 50, 100, 200)) {
    for(j in 1:20) {
      # Erzeuge zufaellige Datensituation
      vec <- sort(runif(i))
      val <- runif(1)
      res <- binarySearch(vec, val)
      # Wenn val ganz Vorne steht muessen alle groesser sein
      if (res$result == 0) {
        expect_true(all(vec > val))
      } else {
        # Ansonsten muss der Vorgaegner kleiner sein
        expect_true(vec[res$result] < val)
      }
      # Wenn val ganz Hinten steht muessen alle kleiner sein
      if (res$result == i) {
        expect_true(all(vec < val))
      } else {
        # Ansonsten muss der Nachfolger groesser sein
        expect_true(vec[res$result + 1] > val)
      }
    }
  }
  # Spezialfaelle: 
  test.case1 <- binarySearch(c(), 1)
  test.case2 <- binarySearch(2, 1)
  test.case3 <- binarySearch(c(1, 2, 2, 3), 2)
  
  expect_equal(test.case1$result, 0)
  expect_equal(test.case1$n.cmps, 0)
  expect_equal(test.case2$result, 0)
  expect_equal(test.case2$n.cmps, 1)
  expect_equal(test.case3$result, 3)
  expect_equal(test.case3$n.cmps, 3)
}
testBinarySearch()

makePlot <- function() {
  ns <- 2^(sort(runif(500, 0, 20)))
  res <- sapply(ns, function(n) binarySearch(runif(n), runif(1))$n.cmps)
  plot(ns, res, log = "x", xlab = "Laenge des Vektor", ylab = "Anzahl Vergleiche")
}

makePlot()
# Treppenfunktion, bei der die einzelnen Treppenstufen sich ueberlappen.
# Bei Logarthmierter x-Achse offensichtlich linearer Anstieg, Blick auf die
# Zahlen bestaetigt die Theorie

# c) und d) s. PDF



## Aufgabe 3: 


# a) s. PDF

# b) und c)

## insertionSort - Sorting function using the Insertion-Sort algorithm
##
## How to count comparisons and switches?
##   comparisons: Every time the head of the while-loop is executed a comparison 
##     is performed. So increase it by 1. If (i != 0) we make one additional
##     comparison, so add one for this case.
##  switches: We simply have to count how many elements had to switch their
##     place with a[j] in each iteration of the for loop. If i = 0 after the
##     while-loop, all elements had to, resulting in (j - 1) switches.
##     For i = 1 the most left element did not switch with a[j], so we have
#      (j - 1 - 1) switches, and so on. So we have (j - i - 1) switches in each
##     for-loop.
##
## Input:
##   a - numeric vector
##
## Output: a named list
##   - sorted.vec: numeric vector - the sorted vector
##   - nof.comps: integer - the number of comparisons
##   - nof.switches: integer - the number of switches

insertionSort <- function(a) {
  
  ## parameter checks
  stopifnot(is.numeric(a), is.vector(a))
  
  n <- length(a)
  ## counter for comparisons and switches
  comps <- 0
  switches <- 0
  
  ## special case
  if(n <= 1L)
    return(list(sorted.vec = a, nof.comps = 0, nof.switches = 0))
  
  for(j in 2:n) {
    tmp <- a[j]
    i <- j - 1
    while(i > 0 && a[i] > tmp) {
      comps <- comps + 1
      a[i + 1] <- a[i]
      i <- i - 1
    }
    
    if(i != 0)
      comps <- comps + 1
    a[i + 1] <- tmp
    switches <- switches + j - i - 1
  }
  
  return(list(sorted.vec = a, nof.comps = comps, nof.switches = switches))
}


## Alternative:
##
## InsertionSort - Sorting function using the Insertion-Sort algorithm
##
## How to count comparisons and switches?
##   comparisons: One comparison is performed in each while-step even if no
##     switch follows. So, for every j we can count how often the while-loop,
##     is executed. However, if it's not executed, we have to add 1 anyway.
##  switches: We simply have to count how many elements had to switch their
##     place with a[j] in each iteration of the for loop. This happens each
##     time the while-loop is executed, so you can simply increase by 1 here.
##
## Input:
##   a - numeric vector
##
## Output: a named list
##   - sorted.vec: numeric vector - the sorted vector
##   - nof.comps: integer - the number of comparisons
##   - nof.switches: integer - the number of switches

InsertionSort <- function(a) {
  
  ## parameter checks
  stopifnot(is.numeric(a), is.vector(a))
  
  n <- length(a)
  ## counter for comparisons and switches
  comps <- numeric(n - 1)
  switches <- 0
  
  ## special case
  if(n <= 1L)
    return(list(sorted.vec = a, nof.comps = 0, nof.switches = 0))
  
  for(j in 2:n) {
    tmp <- a[j]
    i <- j - 1
    while(i > 0 && a[i] > tmp) {
      comps[j - 1] <- comps[j - 1] + 1
      switches <- switches + 1
      a[i + 1] <- a[i]
      i <- i - 1
    }
    a[i + 1] <- tmp
    ## one comparison is performed in each while-step even if no switch follows
    if(comps[j - 1] < j - 1) comps[j - 1] <- comps[j - 1] + 1
  }
  
  return(list(sorted.vec = a, nof.comps = sum(comps), nof.switches = switches))
}


## testsortingFunction - Tests
## Here we use the testthat-package to have a nice testing environment
testsortingFunction <- function(sortingFunction) {
  test_that("Random Vectors", {
    set.seed(1273)
    expect_false(is.unsorted(sortingFunction(sample(100L))$sorted.vec))
    expect_false(is.unsorted(sortingFunction(sample(100L))$sorted.vec))
    expect_false(is.unsorted(sortingFunction(sample(500L))$sorted.vec))
    expect_false(is.unsorted(sortingFunction(sample(500L))$sorted.vec))
    expect_false(is.unsorted(sortingFunction(sample(1000L))$sorted.vec))
    expect_false(is.unsorted(sortingFunction(runif(1000L))$sorted.vec))
    expect_false(is.unsorted(sortingFunction(runif(1000L))$sorted.vec))
  })
  
  test_that("Special Vectors", {
    expect_false(is.unsorted(sortingFunction(1)$sorted.vec))
    expect_equal(sortingFunction(1)$nof.comps, 0)
    expect_equal(sortingFunction(1)$nof.switches, 0)
    expect_false(is.unsorted(sortingFunction(1:100)$sorted.vec))
    expect_equal(sortingFunction(1:100)$nof.switches, 0)
    # expect_equal(sortingFunction(1:100)$nof.comps, 99)
    expect_false(is.unsorted(sortingFunction(100:1)$sorted.vec))
    expect_false(is.unsorted(sortingFunction(rep(1L, 100L))$sorted.vec))
  })
}

testsortingFunction(insertionSort)
testsortingFunction(InsertionSort)

## d) 


## mseq - small helper to create an integer-sequenz from lower to upper
## this overcomes the behavior of R's :-Operator to convert 1:0 into
## the vector [1 0]. Instead here we want 1:0 to be an empty vector
mSeq <- function(lower, upper) {
  if(upper >= lower)
    return(lower:upper)
  else
    return(c())
}

## InsertionSort - Sorting function using the Insertion-Sort algorithm with 
##                  binary search
##
## Input:
##   x - numeric vector
##
## Output: a named list
##   - sorted.vec: numeric vector - the sorted vector
##   - nof.comps: integer - the number of comparisons
##   - nof.switches: integer - the number of switches
insertionSort2 <- function(x) {
  
  ## parameter checks
  stopifnot(is.numeric(x), is.vector(x))
  
  n <- length(x)
  if(n <= 1L)
    return(list(sorted.vec = x, nof.comps = 0, nof.switches = 0))
  
  nof.comps <- 0
  nof.switches <- 0
  
  for(i in 1:(n-1)) {
    # find position to insert x[i+1]:
    tmp <- binarySearch(X = x[1:i], x = x[i+1])
    
    # insert x[i+1] at correct position: 
    x[1:(i+1)] <- c(x[mSeq(1, tmp$result)], x[i+1], x[mSeq(tmp$result+1, i)])
    
    # add number of comparisons from binary search: 
    nof.comps <- nof.comps + tmp$n.cmps
    
    # number of switches = number of elements between new position of x[i+1] and
    # old position i+1:
    nof.switches <- nof.switches + i - tmp$result
  }
  return(list(sorted.vec = x, nof.comps = nof.comps, nof.switches = nof.switches))
}
testsortingFunction(insertionSort2)

## d)

## Laufzeitstudie

## Sort - Sortiert mit einem Sortieralgorithmus 'algo' 10 Vektoren
##        einer festen Laenge 'n' und gibt die mittlere Laufzeit zurueck

count <- function(ns, times, sortingFunction) {
  sapply(ns, function(n)
    rowMeans(replicate(times,
                       unlist(sortingFunction(sample(n))[c("nof.comps", "nof.switches")])
    ))
  )
}


## Grafische Darstellung fuer einen Vektor von ns
visualize <- function(ns, times, sortingFunction) {
  res <- count(ns, times, sortingFunction)
  plot(ns, res[1, ], type = "l",
       ylab = "Mittlere Anzahl Vergleiche", xlab = "n", main = "nof.comps")
  plot(ns, res[2, ], type = "l",
       ylab = "Mittlere Anzahl Vertauschungen", xlab = "n", main = "nof.switches")
  return(res)
}

res_InsertionSort <- visualize(ns = seq(100, 1000, by = 100), times = 10, insertionSort)
# We see: It's quadratic! O(n^2) for both switches and comparisons
res_InsertionSort2 <- visualize(ns = seq(100, 1000, by = 100), times = 10, insertionSort2)
# We see: It's still quadratic, O(n^2), for switches, but the number of comparisons 
# looks nearly linear