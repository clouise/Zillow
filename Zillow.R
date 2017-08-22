library(reshape2)
library(data.table)
library(ggplot2)
library(dplyr)
library(caret)
library(xgboost)
library(Metrics)
options(scipen = 999)
setwd("/Users/carmenlouise/Documents/Zillow")
prop <- fread('/Users/carmenlouise/Documents/Zillow/properties_2016.csv')
t <- fread('/Users/carmenlouise/Documents/Zillow/train_2016_v2.csv')
sample <- fread("/Users/carmenlouise/Documents/Zillow/sample_submission.csv", header = TRUE)

ctypes <- sapply(prop, class)
cidx <- which(ctypes %in% c("character", "factor"))

for(i in cidx){
  prop[[i]] <- as.integer(factor(prop[[i]]))
}


train <- merge(prop, t, by="parcelid", all.y=TRUE)

set.seed(181)

qplot(train$logerror)

# Enable caret to use MAE as eval metric
maeSummary <- function (train,
                        lev = NULL,
                        model = NULL) {
  out <- mae(train$obs, train$pred)  
  names(out) <- "MAE"
  out
}

modelLookup("xgbLinear")

control <- trainControl(method = "cv",
                        number = 5,
                        verboseIter=TRUE,
                        summaryFunction = maeSummary
)

xgb_grid_1 = expand.grid(
  nrounds = c(1000),
  eta = c(.01,.05,.08,.5),
  lambda = c(0,.01,.05,.5,1),
  alpha = c(0,.01,.05,.5,1)
)

traindata <- xgb.DMatrix(as.matrix((train[,2:(ncol(train)-1)])))
target <- train[,logerror]

cb <- train(y=target,
            x=traindata, 
            preProcess=NULL,
            method= "xgbLinear", 
            metric = "MAE", 
            maximize = FALSE, 
            tuneGrid = xgb_grid_1, 
            trControl = control
)

prop2 <- filter(prop,logerror != 0)

print(cb)
cb_prediction <- predict(cb, prop)
predictions <- round(as.vector(cb_prediction), 5)

result <- data.frame(cbind(prop$parcelid, predictions, predictions, predictions, predictions, predictions, predictions))
colnames(result) <- c("parcelid","201610","201611","201612","201710","201711","201712")
write.csv(result, file = "Carmen.csv", row.names = FALSE)
