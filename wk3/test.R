#Part 1
setwd("/Users/dhruvgairola/Documents/Workspace/R/wk3/ProgAssignment3-data")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
names(outcome)
ncol(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])
hist(outcome[, 11], xlab = "30-day Death Rate", main = "Heart Attack 30-day Death Rate")
#Part 2
outcome[, 17] <- as.numeric(outcome[, 17])
outcome[, 23] <- as.numeric(outcome[, 23])
par(mfrow = c(3, 1))
hist(outcome[, 11], xlab = "30-day Death Rate", main = "Heart Attack")
hist(outcome[, 17], xlab = "30-day Death Rate", main = "Heart failure")
hist(outcome[, 23], xlab = "30-day Death Rate", main = "Pneumonia")
y <- c(outcome[, 11], outcome[, 17], outcome[, 23])
range(y, na.rm=TRUE)
hist(outcome[, 11], xlab = "30-day Death Rate", main = "Heart Attack", xlim=c(6, 22))
hist(outcome[, 17], xlab = "30-day Death Rate", main = "Heart failure", xlim=c(6, 22))
hist(outcome[, 23], xlab = "30-day Death Rate", main = "Heart failure", xlim=c(6, 22))
#Part 3
table(outcome$State)
outcome2 <- subset(outcome, table(outcome$State) >= 20)
death <- outcome2[, 11]
state <- outcome2$State
boxplot(death ~ state)
#Part 4
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
hospital <- read.csv("hospital-data.csv", colClasses = "character")
outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")
death <- as.numeric(outcome.hospital[, 11]) ## Heart attack outcome
npatient <- as.numeric(outcome.hospital[, 15])
owner <- factor(outcome.hospital$Hospital.Ownership)
library(lattice)
par(mfrow = c(3, 3))
xyplot(death ~ npatient | owner, data=outcome)
#Part 5