# AoC Day 8 - Part 1 ----------------------------------------------------------------------------------------------

file <- "input.txt"
input <- readChar(file, file.info(file)$size)
input <- as.numeric(unlist(strsplit(input, split = "")))

layers_list <- list()
layer_size <- 25 * 6
number_of_layers <- length(input)/layer_size
# 25 pixels width * 6 pixels height

start <- 1
end <- layer_size

for(i in 1:number_of_layers) {
  layer <- list(input[start:end])
  layers_list <- append(layers_list, layer)
  start <- start + layer_size
  end <- end + layer_size
}

l <- unlist(lapply(layers_list, function(x) length(x[which(x == 0)])))

vec <- layers_list[[which(l == min(l))]]

length(vec[which(vec == 1)]) * length(vec[which(vec == 2)])

# Part 2 ----------------------------------------------------------------------------------------------------------

# 0 = black
# 1 = white
# 2 = transparent

list_of_matrices <-
  lapply(layers_list, function(x) matrix(x, nrow = 6, ncol = 25, byrow = T))
list_of_dataframes <-
  lapply(list_of_matrices, as.data.frame)
df <- data.frame()
for(i in seq_len(length(list_of_dataframes))) {
  df <- rbind(df, list_of_dataframes[[i]])
}

layer <- c()
for(column in seq_len(25)) {
  for(start in seq_len(6)) {
    subset_pixels <- seq(from = start, to = 600, by = 6)
    pixel_found <- FALSE
    for(i in subset_pixels) {
      if (isFALSE(pixel_found)) {
        if (df[i, column] == 2) {
          next
        } else {
          pixel_found <- TRUE
          layer <- append(layer, df[i, column])
        }
      } else {
        df[i, column] <- ""
      }
    }
  }
}

result <- as.data.frame(matrix(layer, nrow = 6, ncol = 25, byrow = F), stringsAsFactors = F)
result[result == 0] <- ""
result
