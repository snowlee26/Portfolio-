#load and pre-processing data
summary(complete)
str(complete)
complete$elevel <- as.factor(complete$elevel)
complete$car <- as.factor(complete$car)
complete$zipcode <- as.factor(complete$zipcode)
complete$brand <- as.factor(complete$brand)

#caret model - Automatic Tuning Grid
#load library and set seed
library(caret)
set.seed(998)

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(complete$brand, p = .75, list = FALSE)
training <- complete[inTraining,]
testing <- complete[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "cv", number = 10)

#train Random Forest Regression model with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)
C50Fit1 <- train(brand~., data = training, method = "C5.0", trControl=fitControl, tuneLength = 1)
                

#training results
C50Fit1

# check the variable importance 
importance <- varImp(C50Fit1)
importance

# import and preprocess the incomplete data
library(readr)
incomplete <- read.csv("SurveyIncomplete.csv")
incomplete$brand <- as.factor(incomplete$brand)
incomplete$elevel <- as.factor(incomplete$elevel)
incomplete$car <- as.factor(incomplete$car)
incomplete$zipcode <- as.factor(incomplete$zipcode)
str(incomplete)

# make a prediction
prediction <- predict(C50Fit1, incomplete)
summary(prediction)
prediction_testing <- predict(C50Fit1, testing)
summary(prediction_testing)

# confusion matrix
confusionMatrix(prediction, incomplete$brand)

# postResample
postResample(prediction, incomplete$brand)
#Accuracy      Kappa 
#0.52940000 0.01016972 
postResample(prediction_testing, testing$brand)

