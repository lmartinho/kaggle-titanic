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
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)

# Understanding Family Size
ggplot(data.combined[1:891,], aes(x = family.size, fill = Survived)) + 
  geom_bar(width = 0.75) +
  facet_wrap(~Pclass + Title) + 
  ggtitle("Pclass, Title") + 
  xlab("family.size") + 
  ylab("Total Count") + 
  ylim(0, 300) +
  labs(fill = "Survived")
