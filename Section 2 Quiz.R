library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers

library(data.table)
heights <- setDT(heights)
head(heights)

#1
heights[, .(avg=mean(height))]
ind = heights$height > 68.3
heights[ind]

#2
heights[ind & sex == "Female"]

#3
dim(heights[sex == "Female"])[1] / dim(heights)[1]

#4
heights[, .(min=min(height))]
match(50, heights$height)
heights[1032, sex]

#5
heights[, .(max=max(height))]
x <- 50:floor(82.7)
x
sum(!x %in% heights$height)

#6
heights2 <- heights[, ht_cm:=height*2.54]
head(heights2)
heights2[18]
heights2[, .(avg=mean(ht_cm))]

#7
females = heights2[sex == "Female"]
head(females)
dim(females)[1]
females[, .(avg=mean(ht_cm))]
