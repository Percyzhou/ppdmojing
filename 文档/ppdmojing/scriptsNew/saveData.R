# 综合全部数据 ----------------------------------------------------------------
train_block <- list(dataMaster_Train, dataLog_Train, dataUserupdate_Train)
test_block <- list(dataMaster_Test, dataLog_Test, dataUserupdate_Test)

dtrain <- Reduce(f = function(x, y) merge(x, y, by = "Idx", all = T), train_block)
dtest <- Reduce(f = function(x, y) merge(x, y, by = "Idx", all = T), test_block)
target <- target
# =============================================================================
# 存储数据集 ------------------------------------------------------------------
saveRDS(dtrain, "dataNew/dataTrain")
saveRDS(dtest, "dataNew/dataTest")
saveRDS(target, "dataNew/targetVar")
# =============================================================================