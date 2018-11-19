# Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add blank labels to the test set
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

# Merge sets into a bigger one
data.combined <- rbind(train, test.survived)

# Start learning more about the data
str(data.combined)

# Machine learning algorithms favor discrete values, so use R's factors (enum)
# Convert chr to factor
data.combined$Survived <- as.factor(data.combined$Survived)
# Convert int to factor
data.combined$Pclass <- as.factor(data.combined$Pclass)

# Take a look at Gross Survival Rates
table(data.combined$Survived)

# Take a look at Pclass
table(data.combined$Pclass)

library(ggplot2)

train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) + 
  geom_bar(width=0.5) + 
  xlab("Pclass") + 
  ylab("Total Count") + 
  labs(fill = "Survived")

head(as.character(train$Name))

length(unique(as.character(data.combined$Name)))

dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

data.combined[which(data.combined$Name %in% dup.names),]

library(stringr)

# Any correlation with other variables (e.g., sibsp)?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

# Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

males = data.combined[which(train$Sex == 'male'),]
males[1:5,]

# Start doing some feature engineering (new Title feature)
extractTitle <- function(name) {
  name = as.character(name)
  
  if (length(grep("Miss.", name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}
titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}
data.combined$Title <- as.factor(titles)

ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
  geom_bar(width = 0.5) + 
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Title") + 
  ylab("Total Count") + 
  labs(fill = "Survived")

# Start of Part 2 of series

ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar(width = 0.5) + 
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Sex") + 
  ylab("Total Count") + 
  labs(fill = "Survived")


summary(data.combined$Age)
summary(data.combined[1:891, "Age"])
summary(test[, "Age"])
summary(train[, "Age"])

# Expanding on the hypothesis regarding title
boys <- data.combined[which(data.combined$Title == 'Master.'),]
summary(boys$Age)

misses <- data.combined[which(data.combined$Title == 'Miss.'),]
summary(misses$Age)

# Check if Misses travelling along are older
misses.alone <- data.combined[which(data.combined$SibSp == 0 & data.combined$Parch == 0),]
summary(misses.alone$Age)

# Understanding Sibsp
data.combined$SibSp <- as.factor(data.combined$SibSp)
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) + 
  geom_bar(width = 0.75) +
  facet_wrap(~Pclass + Title)

str(data.combined$Parch)
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) + 
  geom_bar(width = 0.75) +
  facet_wrap(~Pclass + Title)

temp.sibsp = c(train$SibSp, test$SibSp)
temp.parch = c(train$Parch, test$Parch)
data.combined$FamilySize <- as.factor(temp.sibsp + temp.parch + 1)

# Understanding Family Size
ggplot(data.combined[1:891,], aes(x = FamilySize, fill = Survived)) + 
  geom_bar(width = 0.75) +
  facet_wrap(~Pclass + Title) + 
  ggtitle("Pclass, Title") + 
  xlab("FamilySize") + 
  ylab("Total Count") + 
  ylim(0, 300) +
  labs(fill = "Survived")

# Understanding Family Size
str(data.combined$Ticket)
data.combined$Ticket = as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(ticket.first.char)
data.combined$ticket.first.char <- as.factor(ticket.first.char)

str(data.combined[1:891,])

ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) + 
  geom_bar()

# Trying to find a feature that follows a normal distribution
ggplot(data.combined[1:891,], aes(x = Age)) + 
  geom_bar()
summary(data.combined$Age)

ggplot(data.combined[1:891,], aes(x = Fare)) + 
  geom_bar()
summary(data.combined$Fare)

ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) + 
  geom_bar() + 
  facet_wrap(~Pclass+Title)

# Look into fares
length(unique(data.combined$Fare))
ggplot(data.combined[1:891,], aes(x = Fare)) + 
  geom_bar(width = 0.75)
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) + 
  geom_histogram(binwidth = 5) + 
  facet_wrap(~Pclass + Title) + 
  ylim(0, 50)

str(data.combined$Cabin)
# Cabin isn't really a factor, make it a character object
data.combined$Cabin = as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

table(data.combined$Cabin)

data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin

cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)

data.combined$cabin.first.char <- cabin.first.char

# High level plot
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar()

# Validating intuition about first letters matching upper classes
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() + 
  facet_wrap(~Pclass)


# Validating intuition about first letters matching upper classes
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth=50) + 
  facet_wrap(~Pclass+Title)

# Check whether having multiple cabins booked, increases your chance of survival
cabin.multiple = grepl(" ", data.combined[,"Cabin"])
data.combined$cabin.multiple = cabin.multiple
ggplot(data.combined[1:891,], aes(x = cabin.multiple, fill = Survived)) +
  geom_bar() + 
  facet_wrap(~Pclass+Title)

# Doing it exactly like in the video
data.combined$cabin.multiple.factor = as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))
ggplot(data.combined[1:891,], aes(x = cabin.multiple.factor, fill = Survived)) +
  geom_bar() + 
  facet_wrap(~Pclass+Title)

# Checking whether where you embarked determines your survival rate
ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() + 
  facet_wrap(~Pclass+Title)

# Video #4 - Exploratory Modelling

library(randomForest)

rf.label <- as.factor(train$Survived)
set.seed(1234)

rf.train.1 <- data.combined[1:891, c("Pclass", "Title")]
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

rf.train.2 <- data.combined[1:891, c("Pclass", "Title", "FamilySize")]
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)

rf.train.3 <- data.combined[1:891, c("Pclass", "Title", "FamilySize", "Parch")]
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

rf.train.4 <- data.combined[1:891, c("Pclass", "Title", "FamilySize", "Parch", "SibSp")]
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)

rf.train.5 <- data.combined[1:891, c("Pclass", "Title", "FamilySize", "SibSp")]
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

rf.train.7 <- data.combined[1:891, c("Pclass", "Title", "FamilySize", "SibSp")]
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)

# Prepare predictions for submission
rf.test <- data.combined[892:1309, c("Pclass", "Title", "FamilySize", "SibSp")]
rf.submission <- data.combined[892:1309, c("PassengerId", "Survived")]
rf.submission$Survived <- predict(rf.2, rf.test)
write.csv(rf.submission[, c("PassengerId","Survived")], "submission.csv", row.names = FALSE)

# Introduction to Data Science with R - Cross Validation
# https://www.youtube.com/watch?v=84JSk36og34&t=2110s
# Part 5 of the video series

# Prepare predictions for submission (more like the video series)
test.submit.df <- data.combined[892:1309, c("Pclass", "Title", "FamilySize", "SibSp")]
rf.7.preds = predict(rf.7, test.submit.df)
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.7.preds)
write.csv(submit.df, file = 'submission_alt.csv', row.names = FALSE)

# OOB error estimate: 19.53% (0.8047 score), but submissions score was only 
# 0.76076, so need to introduce cross validation

library(caret) # short for _C_lassification _A_nd _RE_gression _T_raining
library(doSNOW)

set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

# Check stratification
table(rf.label)
342 / 549
table(rf.label[cv.10.folds[[34]]])
308 / 494

# Start using trainControl
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, 
                       index = cv.10.folds)

# MBP: `sysctl hw.physicalcpu hw.logicalcpu`: 4 cpus, 8 threads
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

# 10 fold cross validation, repeated 10 times, with stratified samples
set.seed(3434)
rf.2.cv.1 <- train(x = rf.train.2, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.1)

# Shutdown Cluster
stopCluster(cl)

# Check out results
rf.2.cv.1

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

# Trying with rf.5
set.seed(3434)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.1)

# Shutdown Cluster
stopCluster(cl)

# Trying with 5-fold
set.seed(2348)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)
ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10, index = cv.5.folds)
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)
set.seed(3434)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.2)
stopCluster(cl)
rf.5.cv.1

# Didn't improve, try 3-fold
set.seed(2348)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)
ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.3.folds)
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)
set.seed(3434)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.3)
stopCluster(cl)
rf.5.cv.3
