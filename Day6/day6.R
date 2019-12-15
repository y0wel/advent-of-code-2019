# AoC Day 6 - Part 1 ----------------------------------------------------------------------------------------------

library(igraph)
library(magrittr)

file <- "input.txt"
input <- readChar(file, file.info(file)$size)
input <- unlist(strsplit(input, split = "\n"))
input <- unlist(strsplit(input, split = ")"))
a <- input[seq(1, length(input) - 1, by = 2)]
b <- input[seq(2, length(input), by = 2)]

distances <-
  data.frame(a, b, stringsAsFactors = FALSE) %>%
  graph_from_data_frame()

distances %>%
  distance_table() %>%
  extract2("res") %>%
  sum()

# Part 2 ----------------------------------------------------------------------------------------------------------

distances(
  distances,
  v = V(distances)[["YOU"]],
  to = V(distances)[["SAN"]]
) - 2
