library(dplyr)
library(stringr)
library(caTools)
library(caret)

df <- read.csv('https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv')

maleNobleTitles <- c('Capt', 'Col', 'Don', 'Dr', 'Jonkheer', 'Major', 'Rev', 'Sir')
femaleNobleTitles <- c('Lady', 'Mlle', 'Mme', 'Ms', 'the Countess')

df$Title <- str_sub(df$Name, str_locate(df$Name, ',')[ , 1] + 2, str_locate(df$Name, '\\.')[ , 1] - 1) 
df$Title[df$Title %in% maleNobleTitles] <- 'MaleNoble' 
df$Title[df$Title %in% femaleNobleTitles] <- 'FemaleNoble' 
df$HasCabin <- ifelse(df$Cabin == '', 0, 1) 
df <- df %>% select(-PassengerId, -Name, -Ticket, -Cabin)

lapply(df, function(x) { length(which(is.na(x))) })


df$Age <- ifelse(is.na(df$Age), mean(df$Age, na.rm=TRUE), df$Age)


df$Survived <- factor(df$Survived)
df$Pclass <- factor(df$Pclass)
df$Sex <- factor(df$Sex)
df$SibSp <- factor(df$SibSp)
df$Parch <- factor(df$Parch)
df$Embarked <- factor(df$Embarked)
df$Title <- factor(df$Title)
df$HasCabin <- factor(df$HasCabin)


set.seed(42)

sampleSplit <- sample.split(Y=df$Survived, SplitRatio=0.7)
trainSet <- subset(x=df, sampleSplit==TRUE)
testSet <- subset(x=df, sampleSplit==FALSE)


model <- glm(Survived ~ ., family=binomial(link='logit'), data=trainSet)



probabs <- predict(model, testSet, type='response') 
preds <- ifelse(probabs > 0.5, 1, 0)

install.packages("e1071")

library(e1071)

confusionMatrix(factor(preds), factor(testSet$Survived))
