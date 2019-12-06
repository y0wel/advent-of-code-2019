# AoC Day 5 - Part 1 ----------------------------------------------------------------------------------------------

file <- "input.txt"
input <- readChar(file, file.info(file)$size)
input <- as.numeric(unlist(strsplit(input, split = ",")))
`%nin%` <- Negate(`%in%`)

test_diagnostic_program <- function(input, i = 1, id = 1, out = c()) {
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

test_diagnostic_program(input)

# Part 2 ----------------------------------------------------------------------------------------------------------

test_diagnostic_program <- function(input, i = 1, id = 5, out = c()) {
  while(i < length(input)) {
    if (input[i] == 99) break
    if ((input[i] > 8 | input[i] <= 0) &
        (input[i] %nin% c(101,102,105,106,107,108,1001,1002,1005,1006,1007,1008,1101,1102,1105,1106,1107,1108))) {
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
    if (input[i] == 5) {
      if (input[input[i + 1] + 1] != 0) {
        i <- input[input[i + 2] + 1]
        next
      } else {
        i <- i + 3
        next
      }
    }
    if (input[i] == 6) {
      if (input[input[i + 1] + 1] == 0) {
        i <- input[input[i + 2] + 1]
        next
      } else {
        i <- i + 3
        next
      }
    }
    if (input[i] == 7) {
      result <- ifelse(input[input[i + 1] + 1] < input[input[i + 2] + 1], 1, 0)
    }
    if (input[i] == 8) {
      result <- ifelse(input[input[i + 1] + 1] == input[input[i + 2] + 1], 1, 0)
    }
    if (input[i] == 101) {
      result <- input[i + 1] + input[input[i + 2] + 1]
    }
    if (input[i] == 102) {
      result <- input[i + 1] * input[input[i + 2] + 1]
    }
    if (input[i] == 105) {
      if (input[i + 1] != 0) {
        i <- input[input[i + 2] + 1]
        next
      } else {
        i <- i + 3
        next
      }
    }
    if (input[i] == 106) {
      if (input[i + 1] == 0) {
        i <- input[input[i + 2] + 1]
        next
      } else {
        i <- i + 3
        next
      }
    }
    if (input[i] == 107) {
      result <- ifelse(input[i + 1] < input[input[i + 2] + 1], 1, 0)
    }
    if (input[i] == 108) {
      result <- ifelse(input[i + 1] == input[input[i + 2] + 1], 1, 0)
    }
    if (input[i] == 1001) {
      result <- input[input[i + 1] + 1] + input[i + 2]
    }
    if (input[i] == 1002) {
      result <- input[input[i + 1] + 1] * input[i + 2]
    }
    if (input[i] == 1005) {
      if (input[input[i + 1] + 1] != 0) {
        i <- input[i + 2]
        next
      } else {
        i <- i + 3
        next
      }
    }
    if (input[i] == 1006) {
      if (input[input[i + 1] + 1] == 0) {
        i <- input[i + 2]
        next
      } else {
        i <- i + 3
        next
      }
    }
    if (input[i] == 1007) {
      result <- ifelse(input[input[i + 1] + 1] < input[i + 2], 1, 0)
    }
    if (input[i] == 1008) {
      result <- ifelse(input[input[i + 1] + 1] == input[i + 2], 1, 0)
    }
    if (input[i] == 1101) {
      result <- input[i + 1] + input[i + 2]
    }
    if (input[i] == 1102) {
      result <- input[i + 1] * input[i + 2]
    }
    if (input[i] == 1105) {
      if (input[i + 1] != 0) {
        i <- input[i + 2]
        next
      } else {
        i <- i + 3
        next
      }
    }
    if (input[i] == 1106) {
      if (input[i + 1] == 0) {
        i <- input[i + 2]
        next
      } else {
        i <- i + 3
        next
      }
    }
    if (input[i] == 1107) {
      result <- ifelse(input[i + 1] < input[i + 2], 1, 0)
    }
    if (input[i] == 1108) {
      result <- ifelse(input[i + 1] == input[i + 2], 1, 0)
    }
    if (input[i] %in% c(1,2,7,8,101,102,107,108,1001,1002,1007,1008,1101,1102,1107,1108)) {
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

test_diagnostic_program(input)
