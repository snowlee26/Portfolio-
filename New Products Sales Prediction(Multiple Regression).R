# upload and check existing data set 
library("caret")
str(existing)
summary(existing)
anyNA(existing)

# dummify the data
newDataFrame <- dummyVars(" ~ .", data = existing)
readyExisting <- data.frame(predict(newDataFrame, newdata = existing))

# check data type and NA
str(readyExisting)
summary(readyExisting)
readyExisting$BestSellersRank <- NULL

# correlation
corrMatrix <- cor(readyExisting)
corrMatrix

# corrplot 
install.packages("corrplot")
library(corrplot)
cmPlot <- corrplot(corrMatrix)

# findcorrelation() above 0.9
hc <- findCorrelation(corrMatrix, cutoff = 0.9)
hc <- sort(hc)
hc
#[1] 15 17 18
hc_dataset <- readyExisting[, c(hc)]

# feature selection
reduced_data <- readyExisting[, c(1:12,15,18,20,21,28)]
names(readyExisting)

# set seed
set.seed(520)
inTraing <- createDataPartition(reduced_data$Volume, p = .75, list = FALSE)
training <- reduced_data[inTraing,]
testing <- reduced_data[-inTraing,]

# fit control
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# rf model 
rf_model <- train(Volume~., data = training, method = "rf", trControl = fitControl,
                   importance = T)
rf_model
#RMSE      Rsquared 
#554.5969  0.9481130

# feature importance 
importance <- varImp(rf_model)
importance

# prediction on testing data
rf_prediction <- predict(rf_model, testing)
rf_prediction 
postResample(rf_prediction, testing$Volume)
#RMSE          Rsquared 
#612.5348087   0.9640927



# import and feature select new product data
new$BestSellersRank <- NULL
newDataFrame1 <- dummyVars(" ~ .", data = new)
readyExisting1 <- data.frame(predict(newDataFrame1, newdata = new))
reduced_new <- readyExisting1[, c(1:12,15,18,20,21,28)]
names(readyExisting1)
str(reduced_new)

# prediction on new product 
new_prediction <- round(predict(rf_model, reduced_new))
new_prediction 

# output the prediction for new products
output <- newproductattributes2017 
output$predictions <- new_prediction
output
write.csv(output, file="C2.T3output.csv", row.names = TRUE)






