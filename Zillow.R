library(data.table)
library(caret)
library(xgboost)
library(Metrics)
library(dplyr)
options(scipen = 999)
setwd("/Users/carmenlouise/Documents/Zillow")
prop <- fread('/Users/carmenlouise/Documents/Zillow/properties_2016.csv')
t <- fread('/Users/carmenlouise/Documents/Zillow/train_2016_v2.csv')
#sample <- fread("/Users/carmenlouise/Documents/Zillow/sample_submission.csv", header = TRUE)

#t$month <- as.numeric(substr(t$transactiondate,6,7))
#t %>% group_by(month) %>% dplyr::summarise(logerror = mean(logerror))

# fit <- lm(logerror ~ as.factor(month) +0, data= t)
# summary(fit)
# anova(fit)
# aov <- aov(logerror ~ month, data=t)
# TukeyHSD(aov, conf.level = 0.95)
# 
# ggplot(t, aes(x = as.factor(month), y = logerror)) +
#   geom_boxplot()

t2 <- filter(t, logerror > -.4 | logerror < .42)
rm(t);gc()

# imp_values <- preProcess(prop,
#                          method = c("medianImpute") # May not be needed
# )
# prop <- predict(imp_values, prop)

names <- prop[, -which(colMeans(is.na(prop)) >= 0.8)]
prop <- select(prop,-one_of(names(names)))

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

prop[is.na(prop)] <- 0

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
  subsample = c(.75,.8,.85),
  max_depth = c(5,6,8),
  eta = c(.04,.035,.03,.025),
  colsample_bytree=c(.4,.5),
  min_child_weight=c(4),
  gamma=c(.5)
)

xgb_grid_2 = expand.grid(
  nrounds = c(500),
  subsample = c(.8),
  max_depth = c(6),
  eta = c(.033),
  colsample_bytree=c(.5),
  min_child_weight=c(4),
  gamma=c(.5)
)

traindata <- as.matrix(select(train,-censustractandblock,-transactiondate,-logerror,-parcelid))
target <- train[,logerror]

parcelid <- select(prop,parcelid)
prop2 <- select(prop,-censustractandblock,-parcelid)

rm(t2);rm(prop);gc()
cb <- train(y=target,
            x=traindata, 
            preProcess = NULL,
            method = "xgbTree", 
            metric = "MAE", 
            maximize = FALSE, 
            tuneGrid = xgb_grid_2, 
            trControl = control,
            verbose = TRUE
)

# nrounds = 200, max_depth = 4, eta = 0.04, gamma = 0.5, colsample_bytree = 0.5, min_child_weight = 4 and subsample = 0.75
print(cb)


#prop2$month <- 12
cb_prediction <- predict(cb, prop2)
mean(cb_prediction)
predictions <- round(as.vector(cb_prediction), 4)

result <- data.frame(cbind(parcelid$parcelid, predictions, predictions, predictions, predictions, predictions, predictions))
colnames(result) <- c("parcelid","201610","201611","201612","201710","201711","201712")

t$month <- as.numeric(substr(t$transactiondate,6,7))
t %>% group_by(month) %>% dplyr::summarise(logerror = mean(logerror))

result$`201610` <- result$`201610`*.95+.09*0.016341632
result$`201611` <- result$`201611`*.95+.09*0.014510131
result$`201612` <- result$`201612`*.95+.09*0.019122312

write.csv(result, file = "Carmen_5.csv", row.names = FALSE)





#######################################
df <- fread("carmy.csv")
df <- round(df,4)
df$`201610` <- df$`201610`*.95+.09*0.016341632
df$`201611` <- df$`201611`*.95+.09*0.014510131
df$`201612` <- df$`201612`*.95+.09*0.019122312
mean(df$`201610`)
write.csv(df,"Revision.csv",row.names = FALSE)
