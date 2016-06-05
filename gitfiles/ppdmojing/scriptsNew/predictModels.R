library(xgboost)

dataTest <- readRDS("dataNew/dataTest")
dataTest[is.na(dataTest)] <- -99999
Idx <- dataTest$Idx
dataTest$Idx <- NULL

dataTest <- lapply(dataTest, as.numeric)
dataTest <- do.call("cBind", dataTest)
preds.dt <- predict(model.dt, dataTest, missing = -99999)
preds.lr <- predict(model.lr, dataTest, missing = -99999)
result_table.dt <- data.frame(Idx = Idx, score = preds.dt)
result_table.lr <- data.frame(Idx = Idx, score = preds.lr)
write.csv(result_table.dt, "score/predict779.csv", row.names = F, fileEncoding = "utf-8")
write.csv(result_table.lr, "score/predict75012csv", row.names = F, fileEncoding = "utf-8")
