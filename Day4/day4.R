# AoC Day 4 - Part 1 ----------------------------------------------------------------------------------------------

library(magrittr)

password_range <- seq(359282, 820401, by = 1)
passwords <- c()
digits <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
for(i in 1:length(digits)) {
  number <- gsub(digits[i], "", password_range)
  potential_passwords <- password_range[which(nchar(number) < 5)]
  passwords <- append(passwords, potential_passwords)
}
passwords <- passwords[which(duplicated(passwords) == F)]
rm(number, potential_passwords)

for(i in 1:5) {
  for(j in i:6) {
    input <- lapply(strsplit(as.character(passwords), split = ""), as.numeric)
    condition <- lapply(input, function(x) x[i] <= x[j])
    input <- unlist(lapply(input, paste, collapse = ""))
    condition <- unlist(condition)
    passwords <- input[which(condition == T)]
  }
}
length(passwords)

# Part 2 ----------------------------------------------------------------------------------------------------------

i <- 5
password_subset <-
  gsub(
    paste0("0{",i,"}|1{",i,"}|2{",i,"}|3{",i,"}|4{",i,"}|5{",i,"}|6{",i,"}|7{",i,"}|8{",i,"}|9{",i,"}"),
    "",
    passwords
  )
passwords <- passwords[which(nchar(password_subset) > 1)]

i <- 4
password_subset <-
  gsub(
    paste0("0{",i,"}|1{",i,"}|2{",i,"}|3{",i,"}|4{",i,"}|5{",i,"}|6{",i,"}|7{",i,"}|8{",i,"}|9{",i,"}"),
    "",
    passwords
  )
position <- which(nchar(password_subset) == 2)
test <- lapply(strsplit(password_subset[position], split = ""), function(x) x %>% as.numeric %>% unique)
test <- unlist(lapply(test, function(x) paste(x, collapse = "") %>% as.numeric))
exclude <- position[which(nchar(test) != 1)]
passwords <- passwords[-exclude]

i <- 3
password_subset <-
  gsub(
    paste0("0{",i,"}|1{",i,"}|2{",i,"}|3{",i,"}|4{",i,"}|5{",i,"}|6{",i,"}|7{",i,"}|8{",i,"}|9{",i,"}"),
    "",
    passwords
  )

blank_positions <- which(nchar(password_subset) == 0)
position <- which(nchar(password_subset) == 3)
test <- lapply(strsplit(password_subset[position], split = ""), function(x) x %>% as.numeric %>% unique)
test <- unlist(lapply(test, function(x) paste(x, collapse = "") %>% as.numeric))
exclude <- position[which(nchar(test) == 3)]
exclude <- sort(append(exclude, blank_positions))
passwords <- passwords[-exclude]
length(passwords)
