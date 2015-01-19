library(party)

setwd("~/Centrale/Machine Learning/Bike")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# rbind train and test sets for data prepartation
test$casual <- 0
test$registered   <- 0
test$count <- 0
str(tot)
head(tot)

#factorization of existing factors
tot <- rbind(train, test)
tot$season <- factor(tot$season)
tot$holiday <- factor(tot$holiday)
tot$workingday <- factor(tot$workingday)
tot$weather <- factor(tot$weather)

#create variable time       
tot$time <- sapply(tot$datetime, function(x){substr(x,12,13)})
tot$time <- factor(tot$time)
head(tot$datetime)

#create variable day
tot$day <- sapply(tot$datetime, function(x){weekdays(as.Date(x))})
tot$day <- factor(tot$day)
aggregate(tot[,10:12], by=list(tot$day), FUN=mean)

#create variables saturday and sunday
tot$saturday <- 0
tot$sunday <- 0
tot$saturday[which(tot$day == "samedi")] <- 1
tot$sunday[which(tot$day == "dimanche")] <- 1
tot$saturday <- factor(tot$saturday)
tot$sunday <- factor(tot$sunday)

#create variable hour and daypart
tot$time <- as.numeric(tot$time)
tot$daypart <- 4
tot$daypart[tot$hour >= 4 & tot$hour <= 10] <- 1
tot$daypart[tot$hour >= 11 & tot$hour <= 15] <- 2
tot$daypart[tot$hour >= 16 & tot$hour <= 21] <- 3
tot$daypart <- factor(tot$daypart)
tot$time <- factor(tot$time)

# split in ttrain and test sets
train_factor <- tot[seq(1, nrow(train)),]
test_factor <- tot[-seq(1, nrow(train)),]

#with sunday
fit <- ctree(count ~ season + holiday + workingday + weather + temp + atemp + humidity + time + daypart + sunday, data = train_factor)
prediction <- predict(fit, test_factor)
submit <- data.frame(datetime = test_factor$datetime, count = prediction)
write.csv(submit, file = "withsunday.csv", row.names = F) #Kaggle : 0.49508

#with sunday and saturday
fit <- ctree(count ~ season + holiday + workingday + weather + temp + atemp + humidity + time + daypart + saturday + sunday, data = train_factor)
prediction <- predict(fit, test_factor)
submit <- data.frame(datetime = test_factor$datetime, count = prediction)
write.csv(submit, file = "withsundayandsaturday.csv", row.names = F) # Kaggle : 0.49610

#without sunday
fit <- ctree(count ~ season + holiday + workingday + weather + temp + atemp + humidity + hour + daypart, data = train_factor)
prediction <- predict(fit, test_factor)
submit <- data.frame(datetime = test_factor$datetime, count = prediction)
write.csv(submit, file = "withoutsunday.csv", row.names = F) # Kaggle : 0.56145

#registered and casual
fitR <- ctree(registered ~ season + holiday + workingday + weather + temp + atemp + humidity + hour + daypart + sunday, data = train_factor)
predictionR <- predict(fitR, test_factor)
fitC <- ctree(casual ~ season + holiday + workingday + weather + temp + atemp + humidity + hour + daypart + sunday, data = train_factor)
predictionC <- predict(fitC, test_factor)
prediction <- predictionC + predictionR
submit <- data.frame(datetime = test_factor$datetime, count = prediction)
colnames(submit) <- c("datetime", "count")
write.csv(submit, file = "registeredPLUScasual.csv", row.names = F) # Kaggle : 0.51220
