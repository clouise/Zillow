library(reshape2)
library(data.table)
library(ggplot2)
library(caret)
library(xgboost)
library(Metrics)
library(dplyr)
options(scipen = 999)
setwd("/Users/carmenlouise/Documents/Zillow")
prop <- fread('/Users/carmenlouise/Documents/Zillow/properties_2016.csv')
t <- fread('/Users/carmenlouise/Documents/Zillow/train_2016_v2.csv')
#sample <- fread("/Users/carmenlouise/Documents/Zillow/sample_submission.csv", header = TRUE)

t2 <- t %>% group_by(parcelid) %>% dplyr::summarise(logerror = mean(logerror))

t2 <- filter(t2, logerror > .4 | logerror < .4)

prop$airconditioningtypeid[is.na(prop$airconditioningtypeid)] <- 0
prop$architecturalstyletypeid[is.na(prop$architecturalstyletypeid)] <- 0
prop$buildingclasstypeid[is.na(prop$buildingclasstypeid)] <- 0
prop$buildingqualitytypeid[is.na(prop$buildingqualitytypeid)] <- 0
prop$propertylandusetypeid[is.na(prop$propertylandusetypeid)] <- 0
prop$storytypeid[is.na(prop$storytypeid)] <- 0
prop$typeconstructiontypeid[is.na(prop$typeconstructiontypeid)] <- 0

imp_values <- preProcess(prop,
                         method = c("medianImpute") # May not be needed
)
prop <- predict(imp_values, prop)

prop$longmean <- mean((prop$longitude), na.rm=TRUE)/1e6
prop$latmean <- mean((prop$latitude), na.rm=TRUE)/1e6
prop$longitude1 <- prop$longitude/1e6
prop$latitude1 <- prop$latitude/1e6

prop[,longmean := NULL]
prop[,latmean := NULL]
prop[,longitude1 := NULL]
prop[,latitude1 := NULL]
prop[, landValRatio := (prop$landtaxvaluedollarcnt / (prop$landtaxvaluedollarcnt + prop$structuretaxvaluedollarcnt))]
prop[, bathInteraction := (prop$bathroomcnt * prop$calculatedfinishedsquarefeet)]
prop[, sqftRoom := (prop$calculatedfinishedsquarefeet / prop$roomcnt)]
prop[, strucLand := (prop$calculatedfinishedsquarefeet / prop$lotsizesquarefeet)]
prop[, age := 2020 - prop$yearbuilt]

ctypes <- sapply(prop, class)
cidx <- which(ctypes %in% c("character", "factor"))

for(i in cidx){
  prop[[i]] <- as.integer(factor(prop[[i]]))
}

rm(t);gc()
train <- merge(prop, t2, by="parcelid", all.y=TRUE)

set.seed(181)
# Enable caret to use MAE as eval metric
maeSummary <- function (train,
                        lev = NULL,
                        model = NULL) {
  out <- mae(train$obs, train$pred)  
  names(out) <- "MAE"
  out
}

modelLookup("xgbTree")
control <- trainControl(method = "cv",
                        number = 5,
                        verboseIter=TRUE,
                        summaryFunction = maeSummary
)

xgb_grid_1 = expand.grid(
  nrounds = c(500),
  subsample = c(.75),
  max_depth = c(5),
  eta = c(.06),
  colsample_bytree=c(.5),
  min_child_weight=c(4),
  gamma=c(.5)
)

traindata <- as.matrix(select(train,-parcelid,-censustractandblock,-logerror))
target <- train[,logerror]

cb <- train(y=target,
            x=traindata, 
            preProcess=NULL,
            method= "xgbTree", 
            metric = "MAE", 
            maximize = FALSE, 
            tuneGrid = xgb_grid_1, 
            trControl = control
)

print(cb)

prop2 <- select(prop,-parcelid,-censustractandblock)

cb_prediction <- predict(cb, prop2)
cb_prediction
mean(cb_prediction)
predictions <- round(as.vector(cb_prediction), 4)

result <- data.frame(cbind(prop$parcelid, predictions, predictions, predictions, predictions, predictions, predictions))
colnames(result) <- c("parcelid","201610","201611","201612","201710","201711","201712")
write.csv(result, file = "Carmen_3.csv", row.names = FALSE)
