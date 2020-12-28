# https://www.r-bloggers.com/2020/05/step-by-step-guide-on-how-to-build-linear-regression-in-r-with-code/

# Reading data
housing <- read.csv("C:/Users/pbaum/OneDrive/HarvardX Data Science 2020/Path/Holiday/Screenshots/R LinRegression/USA_Housing.csv",
                    header = TRUE, sep = ",")

#Address not needed
housing$Address <- NULL

# Print top 6 observations
head(housing)

#install.packages("ggplot2")
library(ggplot2)
# Building histogram
ggplot(data=housing, aes(Price)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

# loading psych package
#install.packages('psych')
library(psych)
psych::describe(housing)

#install.packages("reshape")
library(reshape)
meltData <- melt(housing)
p <- ggplot(meltData, aes(factor(variable), value))
p + geom_boxplot() + facet_wrap(~variable, scale="free")


#install.packages("corrgram")
require(corrgram)
corrgram(housing, order=TRUE)

#install.packages("caret")
library(caret)
# Split data into train and test
index <- createDataPartition(housing$Price, p = .70, list = FALSE)
train <- housing[index, ]
test <- housing[-index, ]
# Checking the dim of train
dim(train)

# Taining model
lmModel <- lm(Price ~ . , data = train)
# Printing the model object
print(lmModel)

# Checking model statistics
summary(lmModel)

