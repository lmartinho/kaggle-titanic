# Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

data.combined <- rbind(train, test.survived)

str(data.combined)

# Convert chr to factor
data.combined$Survived <- as.factor(data.combined$Survived)
# Convert int to factor
data.combined$Pclass <- as.factor(data.combined$Pclass)

# Gross Survival Rates
table(data.combined$Survived)
