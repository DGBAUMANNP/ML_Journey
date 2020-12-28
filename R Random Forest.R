path <- "C:/Users/baumannp/OneDrive/HarvardX Data Science 2020/Path/Holiday/Screenshots/R Random Forest/cars.csv"

# Data Source: https://archive.ics.uci.edu/ml/machine-learning-databases/car/

install.packages("randomForest")
library(randomForest)

# Load the dataset and explore
data1 <- read.csv(path, sep=";")
  
head(data1)
  
str(data1)
  
summary(data1)

glimpse(data1)


data1$BuyingPrice <- factor(data1$BuyingPrice)
data1$Maintenance <- factor(data1$Maintenance)
data1$NumDoors <- factor(data1$NumDoors)
data1$NumPersons <- factor(data1$NumPersons)
data1$BootSpace <- factor(data1$BootSpace)
data1$Safety <- factor(data1$Safety)
data1$Condition <- factor(data1$Condition)


# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
TrainSet <- data1[train,]
ValidSet <- data1[-train,]

summary(TrainSet)
summary(ValidSet)

# Create a Random Forest model with default parameters
model1 <- randomForest(Condition ~ ., data = TrainSet, importance = TRUE)
model1

# Fine tuning parameters of Random Forest model
set.seed(101)
model2 <- randomForest(Condition ~ ., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)
model2

# Predicting on train set
predTrain <- predict(model2, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$Condition)  


# Predicting on Validation set
predValid <- predict(model2, ValidSet, type = "class")
# Checking classification accuracy
mean(predValid == ValidSet$Condition)                    
table(predValid,ValidSet$Condition)


# To check important variables
importance(model2)        
varImpPlot(model2)   


# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(Condition ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = "class")
  a[i-2] = mean(predValid == ValidSet$Condition)
}
a
plot(3:8,a)






