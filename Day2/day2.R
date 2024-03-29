# AoC Day 2 - Part 1 ----------------------------------------------------------------------------------------------

input <-
  c(1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,6,19,23,2,23,6,27,1,5,27,31,1,10,31,35,2,6,35,
    39,1,39,13,43,1,43,9,47,2,47,10,51,1,5,51,55,1,55,10,59,2,59,6,63,2,6,63,67,1,5,67,71,2,9,
    71,75,1,75,6,79,1,6,79,83,2,83,9,87,2,87,13,91,1,10,91,95,1,95,13,99,2,13,99,103,1,103,10,
    107,2,107,10,111,1,111,9,115,1,115,2,119,1,9,119,0,99,2,0,14,0)


restore_gravity_assist_program <- function(input, i = 1) {
  while(i < length(input)) {
    if (input[i] == 99) break
    if (input[i] > 2 | input[i] == 0) {
      i <- i + 1
      next
    }
    if (input[i] == 1) {
      result <- input[input[i + 1] + 1] + input[input[i + 2] + 1]
    }
    if (input[i] == 2) {
      result <- input[input[i + 1] + 1] * input[input[i + 2] + 1]
    }
    if (input[i + 3] == 0) {
      input[1] <- result
    } else {
      input[input[i + 3] + 1] <- result
    }
    i <- i + 4
  }
  return(input)
}

restore_gravity_assist_program(input)[1]

# Part 2 ----------------------------------------------------------------------------------------------------------

reset_input <- function(noun, verb) {
  c(1,noun,verb,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,6,19,23,2,23,6,27,1,5,27,31,1,10,31,35,2,6,35,
    39,1,39,13,43,1,43,9,47,2,47,10,51,1,5,51,55,1,55,10,59,2,59,6,63,2,6,63,67,1,5,67,71,2,9,
    71,75,1,75,6,79,1,6,79,83,2,83,9,87,2,87,13,91,1,10,91,95,1,95,13,99,2,13,99,103,1,103,10,
    107,2,107,10,111,1,111,9,115,1,115,2,119,1,9,119,0,99,2,0,14,0)
}

refine_gravity_assist_program <- function(input) {
  for(noun in 0:99) {
    for(verb in 0:99) {
      input <- reset_input(noun = noun, verb = verb)
      result <- restore_gravity_assist_program(input)[1]
      if (result == 19690720) {
        return(c(noun, verb))
      }
    }
  }
}

result <- refine_gravity_assist_program(input)
100 * result[1] + result[2]
