setwd("/Users/darshiks/Downloads/all")

# Loading packages
library(data.table)
library(randomForest)
library(MASS)
library(caTools)
library(ggplot)
library(sqldf)
library(gbm)
library(xgboost)
library(tidyverse)
library(caretEnsemble)
library(survival)
library(dplyr)
library(caret)
library(party)


# Reading the traning set and performing exploratory analysis
training_file <- read.csv("train.csv", header = TRUE)
dim(training_file)
str(training_file)
summary(training_file)
head(training_file)

# Outlier detection:
## 1. Check the distribution of Sale price i.e. the dependent variable
qplot(training_file$SalePrice, geom="histogram")

# The data is mostly normal very lightly skewed to the right due to outliers. The author of the
# dataset recommends to remove the houses greater than 4000 square feet from the data set
plot(training_file$GrLivArea, training_file$SalePrice)

# The chart clearly shows the outliers i.e. some house with a very large living area and very low house prices, we remove
# observations with Area >4000 and house price less than 200k
training_file <- training_file %>% filter(GrLivArea<4000,SalePrice>200000)

#Correlation Analysis - Since we have a large number of numerical variables, we don't want to use the features which
# are highly correlated with each other. Let's draw a heatmap of correlated features to identify the top correlated features
numeric_varibles <- training_file[,c(4,18,38,44,45,46,47,48,49,50,51,52,53,55,81)]
head(numeric_varibles)
library(reshape2)
cormat <- round(cor(numeric_varibles),2)
melted_cormat <- melt(cormat)
head(melted_cormat)
library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

## We can decipher from the correlation heat map that the OverallQual and GrLivArea  
# are highly positively correlated with the SalePrice. We are not going to drop correlated variable for now since
# random forest handles correlation pretty well

# Handling Missing values- Removing columns with more than 50% Null Values
summary(training_file)
training_file$Alley<- NULL
training_file$Fence<- NULL
training_file$PoolQC<- NULL
training_file$MiscFeature<- NULL
training_file$Id<- NULL

#Changing character to factor- already done while reading the file

#Creating train and test dataset
set.seed(123)
ind <- sample.split(Y = training_file$SalePrice, SplitRatio = 0.7)
trainDF<- training_file[ind,]
testDf <- training_file[!ind,]

summary(trainDF)
summary(testDf)

#Removing actual target column
trainDF$SaleDollarCnt <- NULL


#Random Forest Implementation
#======================================================
train_data_rf <- trainDF
test_data_rf <- testDf

house_model_rf <- randomForest(SalePrice~., data = train_data_rf, mytry = 5, ntree = 200,na.action = na.omit)
importance    <- importance(house_model_rf)
varImpPlot(house_model_rf)

# Creating predictions

test_data_rf$prediction <- predict(house_model_rf,test_data_rf)

# Taking antilog of the predictions
#test_data_rf$antilog_SaleDollarCnt <-  exp(test_data_rf$prediction)
summary(train_data_rf)
summary(test_data_rf)

#write.table(test_data_rf, file = 'test_data_8.txt', quote = F, row.names = F, col.names = T)

test_data_rf$percentage_change_rf <- (!is.na(test_data_rf$SalePrice-test_data_rf$prediction))/test_data_rf$SalePrice
test_data_rf$abs_percentage_change_rf <- abs(test_data_rf$percentage_change_rf)

mean_abs_per_ch_rf <- mean(test_data_rf$abs_percentage_change_rf)*100
med_abs_per_ch_rf <- median(test_data_rf$abs_percentage_change_rf)*100

mean_abs_per_ch_rf
med_abs_per_ch_rf

hist(test_data_rf$abs_percentage_change_rf)

# Actual predictions in the test file
test_file <- read.csv("test.csv", header = TRUE)
summary(test_file)
str(train_data_rf)

# Need to set the levels of test file as same as the train file
levels(test_file$MSZoning) <- levels(train_data_rf$MSZoning)
levels(test_file$Street) <- levels(train_data_rf$Street)
levels(test_file$LotShape) <- levels(train_data_rf$LotShape)
levels(test_file$LandContour) <- levels(train_data_rf$LandContour)
levels(test_file$Utilities) <- levels(train_data_rf$Utilities)
levels(test_file$LotConfig) <- levels(train_data_rf$LotConfig)
levels(test_file$LandSlope) <- levels(train_data_rf$LandSlope)
levels(test_file$Neighborhood) <- levels(train_data_rf$Neighborhood)
levels(test_file$Condition1) <- levels(train_data_rf$Condition1)
levels(test_file$Condition2) <- levels(train_data_rf$Condition2)
levels(test_file$BldgType) <- levels(train_data_rf$BldgType)
levels(test_file$HouseStyle) <- levels(train_data_rf$HouseStyle)
levels(test_file$RoofStyle) <- levels(train_data_rf$RoofStyle)
levels(test_file$RoofMatl) <- levels(train_data_rf$RoofMatl)
levels(test_file$Exterior1st) <- levels(train_data_rf$Exterior1st)
levels(test_file$Exterior2nd) <- levels(train_data_rf$Exterior2nd)
levels(test_file$MasVnrType) <- levels(train_data_rf$MasVnrType)
levels(test_file$ExterQual) <- levels(train_data_rf$ExterQual)
levels(test_file$ExterCond) <- levels(train_data_rf$ExterCond)
levels(test_file$Foundation) <- levels(train_data_rf$Foundation)
levels(test_file$BsmtQual) <- levels(train_data_rf$BsmtQual)
levels(test_file$BsmtCond) <- levels(train_data_rf$BsmtCond)
levels(test_file$BsmtExposure) <- levels(train_data_rf$BsmtExposure)
levels(test_file$BsmtFinType1) <- levels(train_data_rf$BsmtFinType1)
levels(test_file$BsmtFinType2) <- levels(train_data_rf$BsmtFinType2)
levels(test_file$Heating) <- levels(train_data_rf$Heating)
levels(test_file$HeatingQC) <- levels(train_data_rf$HeatingQC)
levels(test_file$CentralAir) <- levels(train_data_rf$CentralAir)
levels(test_file$Electrical) <- levels(train_data_rf$Electrical)
levels(test_file$KitchenQual) <- levels(train_data_rf$KitchenQual)
levels(test_file$FireplaceQu) <- levels(train_data_rf$FireplaceQu)
levels(test_file$GarageType) <- levels(train_data_rf$GarageType)
levels(test_file$GarageFinish) <- levels(train_data_rf$GarageFinish)
levels(test_file$GarageQual) <- levels(train_data_rf$GarageQual)
levels(test_file$GarageCond) <- levels(train_data_rf$GarageCond)
levels(test_file$PavedDrive) <- levels(train_data_rf$PavedDrive)
levels(test_file$SaleType) <- levels(train_data_rf$SaleType)
levels(test_file$SaleCondition) <- levels(train_data_rf$SaleCondition)


test_file$SalePrice <- predict(house_model_rf,test_file)
summary(test_file$SalePrice)

output <- test_file[,c('Id','SalePrice')]
head(output)
write.csv(output,"output.csv")

