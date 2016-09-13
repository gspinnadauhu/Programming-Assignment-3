#1 Plot 30 day mortality rates for heart attack

#loading data
outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
head(outcome)

#histogram of 30-day death rates
outcome[,11] <- as.numeric(outcome[,11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[,11],main="30-day Death rates from heart attack",xlab="day")