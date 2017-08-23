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

t$month <- as.numeric(substr(t$transactiondate,6,7))
# fit <- lm(logerror ~ as.factor(month) +0, data= t)
# summary(fit)
# anova(fit)
# aov <- aov(logerror ~ month, data=t)
# TukeyHSD(aov, conf.level = 0.95)
# 
# ggplot(t, aes(x = as.factor(month), y = logerror)) +
#   geom_boxplot()

t2 <- filter(t, logerror > -.4 | logerror < .4)
rm(t);gc()

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
  nrounds = c(200),
  subsample = c(.75,.78,.8),
  max_depth = c(4,5,6),
  eta = c(.06,.05,.04,.03),
  colsample_bytree=c(.5),
  min_child_weight=c(1,4),
  gamma=c(.5)
)

xgb_grid_2 = expand.grid(
  nrounds = c(300),
  subsample = c(.75),
  max_depth = c(4),
  eta = c(.04),
  colsample_bytree=c(.5),
  min_child_weight=c(4),
  gamma=c(.5)
)

traindata <- as.matrix(select(train,-censustractandblock,-transactiondate,-logerror,-parcelid))
target <- train[,logerror]

rm(t2);gc()
cb <- train(y=target,
            x=traindata, 
            preProcess=NULL,
            method= "xgbTree", 
            metric = "MAE", 
            maximize = FALSE, 
            tuneGrid = xgb_grid_2, 
            trControl = control
)

# nrounds = 200, max_depth = 4, eta = 0.04, gamma = 0.5, colsample_bytree = 0.5, min_child_weight = 4 and subsample = 0.75
print(cb)
prop2 <- select(prop,-censustractandblock,-parcelid)

rm(prop);gc()

prop2$month <- 10
cb_prediction2 <- predict(cb, prop2)
cb_prediction2
mean(cb_prediction2)
predictions1 <- round(as.vector(cb_prediction1), 4)

result <- data.frame(cbind(prop$parcelid, predictions1, predictions2, predictions3, predictions1, predictions2, prediction3))
colnames(result) <- c("parcelid","201610","201611","201612","201710","201711","201712")
write.csv(result, file = "Carmen_4.csv", row.names = FALSE)
