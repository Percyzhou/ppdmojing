library(xgboost)
# logregobj <- function(preds, dtrain) {
#   labels <- getinfo(dtrain, "label")
#   preds <- 1/(1 + exp(-preds))
#   grad <- preds - labels
#   hess <- preds * (1 - preds)
#   return(list(grad = grad, hess = hess))
# }

dataTrain <- readRDS("dataNew/dataTrain")
dataTrain <- dataTrain[order(dataTrain$Idx),]

dataTrain$Idx <- NULL
dataTrain_names <- names(dataTrain)
dataTrain[is.na(dataTrain)] <- -99999

targetVar <- readRDS("dataNew/targetVar")
targetVar <- targetVar$target
params <- list(
  booster = "gblinear",
  
  lambda = 1,
  alpha = 10,
  lambda_bias = 1,

  objective = 'binary:logistic',
  eval_metric = "auc",
  nthread = 4
)
dataTrain <- lapply(dataTrain, as.numeric)
dataTrain <- do.call("cBind", dataTrain)
dataTrain <- xgb.DMatrix(dataTrain, label = targetVar, missing = -99999)
model.lr <- xgb.train(dataTrain, params = params, nrounds = 800)
# xgb_dtree <- xgb.model.dt.tree(feature_names = dataTrain_names, model = model1)
# xgb_importance <- xgb.importance(feature_names = dataTrain_names, model = model1)
# model2 <- xgb.train(dataTrain, params = params, nrounds = 250)
# model3 <- xgb.train(dataTrain, params = params, nrounds = 500)
# xgb_importance1 <- xgb.importance(feature_names = dataTrain_names, model = model1)
# xgb_importance2 <- xgb.importance(feature_names = dataTrain_names, model = model2)
# xgb_importance3 <- xgb.importance(feature_names = dataTrain_names, model = model3)
# set.seed(15469)
# history <- xgb.cv(data = dataTrain, nrounds = 800,
#                   params = params, nfold = 4)

