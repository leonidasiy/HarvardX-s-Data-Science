library(dslabs)
data(heights)

library(data.table)
heights <- setDT(heights)
head(heights)

#1
sum(ifelse(heights$sex == "Female", 1, 2))

#2
mean(ifelse(heights$height > 72, heights$height, 0))

#3
inches_to_ft <- function(x) {
  x / 12
}
inches_to_ft(144)
heights[inches_to_ft(height) < 5]
