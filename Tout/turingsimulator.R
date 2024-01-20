## runTuringMachine - runs the simulator for a generic turing machine
##
## Input
##   I       - the instruction set. Must be a list, that contains a 
##             list for every instruction. Every single instruction is
##             a list with 5 entries (in this order)
##             * The cell number
##             * The character from the tape
##             * The character, which will be printed on the tape 
##             * The instruction for the cell selector, must be "L",
##               "R" or "N"
##             * The next cell number
##   tape    - the initial tape of the TM
##   L0      - the initial instruction of the TM
##   F       - the final states of the TM
##   ini.pos - the initiel position of the RW
##
## Output
##   No output, but as a side effect after every step the tape is 
##   printed on the R console
runTuringMachine <- function(I, tape, L0, F, ini.pos) {
  ## Save the current of the TM in a list. This list contains:
  ## cell.selector - the current value of the cell selector
  ## tape          - the current tape
  ## pos           - the current position of the RW
  tm <- list(cell.selector = L0, tape = tape, pos = ini.pos)
  repeat {
    ## some nice output on the console
    tape.str <- paste(" ", paste(tm$tape, collapse = " "), sep = "")
    i <- tm$pos * 2 
    substr(tape.str, i - 1, i - 1) = "*"
    substr(tape.str, i + 1, i + 1) = "*"
    cat("tape:", tape.str , " cell.selector:", tm$cell.selector)
    ## error, if the machine runs of the tape
    if (tm$pos < 1 || tm$pos > length(tape))
      stop("The TM ran of the tape")
    ## stop if final state is reaches
    if(tm$cell.selector %in% F)
      break  
    ## execute one Step
    tm <- oneStep(I, tm$tape, tm$cell.selector, tm$pos)
  }
  cat("\n")
  return(invisible(tm$tape))
}

## oneStep - executes one Step of a given Turing Machine.
##
## Input
##   I             - the instruction set, as described in the 
##                   documentation of turingMachine
##   tape          - the current tape of the TM
##   cell.selector - (cell selector) the current state of the TM
##   pos           - the current position of the RW
##
## Output
##   A list with the named elements
##   tape  - the tape of the TM after the execution of the step
##   state - the new state of the TM
##   pos   - the new position of the RW
oneStep <- function(I, tape, cell.selector, pos) {
  ## 1st cycle
  command.register <- Filter(function(p) p[[1]] == cell.selector, I)
  ## 2nd cycle
  input.register <- tape[pos]
  ## 3rd cycle
  tmp <- Find(function(p) p[[2]] == input.register, command.register)
  operation.register <- tmp[3:4]
  cell.selector <- tmp[[5]]
  ## some nice output
  cat("  next:", unlist(tmp), "\n")
  ## 4th cycle
  tape[pos] <- operation.register[[1]]
  ## 5th cycle
  pos <- if(operation.register[[2]] == "L") pos - 1L else pos
  pos <- if(operation.register[[2]] == "R") pos + 1L else pos
  ## return the new TM state
  return(list(cell.selector = cell.selector, tape = tape, pos = pos))
}

## Example: The Adding TM from the book
prog.add <- list(
  list(1, "b", "b", "R", 2),
  list(1, "I", "b", "R", 0),
  list(2, "b", "I", "R", 3),
  list(2, "I", "I", "R", 2),
  list(3, "b", "b", "L", 4),
  list(3, "I", "I", "R", 3),
  list(4, "b", "b", "R", 0),
  list(4, "I", "b", "L", 5),
  list(5, "b", "b", "R", 0),
  list(5, "I", "b", "L", 6),
  list(6, "b", "b", "R", 0),
  list(6, "I", "I", "L", 6)
)
## initiate the tape and run machine
ini.tape <- c("b", "I", "b", "I", "I", "b")
runTuringMachine(prog.add, tape = ini.tape, L0 = 1L, F = 0L,
  ini.pos = 1L)
