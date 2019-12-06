# AoC Day 5 - Part 1 ----------------------------------------------------------------------------------------------

file <- "/Users/tobiasmatz/Desktop/input.txt"
input <- readChar(file, file.info(file)$size)
input <- as.numeric(unlist(strsplit(input, split = ",")))
`%nin%` <- Negate(`%in%`)

restore_gravity_assist_program <- function(input, i = 1, id = 1, out = c()) {
  while(i < length(input)) {
    if (input[i] == 99) break
    if ((input[i] > 4 | input[i] <= 0) &
        (input[i] %nin% c(101,102,1001,1002,1101,1102))) {
      i <- i + 1
      next
    }
    if (input[i] == 1) {
      result <- input[input[i + 1] + 1] + input[input[i + 2] + 1]
    }
    if (input[i] == 2) {
      result <- input[input[i + 1] + 1] * input[input[i + 2] + 1]
    }
    if (input[i] == 3) {
      if (input[i + 1] == 0) {
        input[1] <- id
      } else {
        input[input[i + 1] + 1] <- id
      }
    }
    if (input[i] == 4) {
      if (input[i + 1] == 0) {
        out <- append(out, input[1])
      } else {
        out <- append(out, input[input[i + 1] + 1])
      }
    }
    if (input[i] == 101) {
      result <- input[i + 1] + input[input[i + 2] + 1]
    }
    if (input[i] == 102) {
      result <- input[i + 1] * input[input[i + 2] + 1]
    }
    if (input[i] == 1001) {
      result <- input[input[i + 1] + 1] + input[i + 2]
    }
    if (input[i] == 1002) {
      result <- input[input[i + 1] + 1] * input[i + 2]
    }
    if (input[i] == 1101) {
      result <- input[i + 1] + input[i + 2]
    }
    if (input[i] == 1102) {
      result <- input[i + 1] * input[i + 2]
    }
    if (input[i] == 1 | input[i] == 2 | (input[i] %in% c(101,102,1001,1002,1101,1102))) {
      if (input[i + 3] == 0) {
        input[1] <- result
      } else {
        input[input[i + 3] + 1] <- result
      }
      i <- i + 4
      next
    }
    if (input[i] == 3 | input[i] == 4) {
      i <- i + 2
      next
    }
  }
  return(out)
}

restore_gravity_assist_program(input)

# Part 2 ----------------------------------------------------------------------------------------------------------
