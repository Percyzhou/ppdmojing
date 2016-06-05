library(xgboost)
library(rpart)
cvXgboost <- function(data, target, params){
  data$Idx <- NULL
  data_names <- names(data)
  data[is.na(data)] <- -99999
  target <- target$target
  data <- lapply(data, as.numeric)
  data <- do.call("cBind", data)
  data <- xgb.DMatrix(data, label = target, missing = -99999)
  set.seed(15469)
  history <- xgb.cv(data = data, nrounds = 800,
                    params = params, nfold = 4)
}

feature_Xgboost <- function(data, target, params){
  data$Idx <- NULL
  data_names <- names(data)
  data[is.na(data)] <- -99999
  target <- target$target
  data <- lapply(data, as.numeric)
  data <- do.call("cBind", data)
  data <- xgb.DMatrix(data, label = target, missing = -99999)
  model <- xgb.train(data, params = params, nrounds = 700)
  xgb.importance(feature_names = data_names, model = model)
}

dataTrain <- readRDS("dataNew/dataTrain")
dataTrain <- dataTrain[order(dataTrain$Idx),]
targetVar <- readRDS("dataNew/targetVar")

a_Idx <- read.csv("data/Test_A/a_testMaster1.csv", fileEncoding = "gbk", stringsAsFactors = F)$Idx
b_Idx <- read.csv("data/Test_A/a_testMaster2.csv", fileEncoding = "gbk", stringsAsFactors = F)$Idx

a_dataTrain <- dataTrain[dataTrain$Idx %in% a_Idx, ]
a_target <- target[targetVar$Idx %in% a_Idx, ]

b_dataTrain <- dataTrain[dataTrain$Idx %in% b_Idx, ]
b_target <- target[targetVar$Idx %in% b_Idx, ]

params <- list(
  max.depth = 8,
  eta = 0.5,
  subsample = 0.7,
  colsample_bytree = 0.3,
  gamma = 0.1,
  scale_pos_weight = sum(targetVar == 0) / sum(targetVar == 1),
  lambda = 20000,
  alpha = 10,
  
  min_child_weight = 8,
  max_delta_step = 5,
  objective = 'binary:logistic',
  eval_metric = "auc",
  nthread = 4
)

cvXgboost(a_dataTrain, a_target, params = params)
cvXgboost(b_dataTrain, b_target, params = params)

a_featureScore <- feature_Xgboost(a_dataTrain, a_target, params = params)
b_featureScore <- feature_Xgboost(a_dataTrain, a_target, params = params)