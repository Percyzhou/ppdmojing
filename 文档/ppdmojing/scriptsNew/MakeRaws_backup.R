# 读取Master数据 -----------------------------------------------------------------
dataMaster1_1 <- read.csv("data/Train/trainMaster.csv", fileEncoding = "gbk", stringsAsFactors = F)
dataMaster1_2 <- read.csv("data/Test_A/a_testMaster1.csv", fileEncoding = "gbk", stringsAsFactors = F)
dataMaster1_3 <- read.csv("data/Test_A/a_testMaster2.csv", fileEncoding = "gbk", stringsAsFactors = F)
dataMaster1 <- rBind(dataMaster1_1, dataMaster1_2, dataMaster1_3)
dataMaster2 <- read.csv("data/Test_B/b_testMaster.csv", fileEncoding = "gbk", stringsAsFactors = F)

target <- dataMaster1[, c("Idx", "target")]
target <- target[order(dataMaster1$Idx),]
dataMaster1$target <- NULL

train_Idx <- dataMaster1$Idx
test_Idx <- dataMaster2$Idx
dataMaster <- rBind(dataMaster1, dataMaster2)
# ================================================================================
config <- read.xlsx("config/魔镜杯字段类型说明文档.xlsx", sheetIndex = 1, header = T, as.data.frame = T, stringsAsFactors = F)
config <- subset(config, !(变量名称 == "target" | 变量名称 == "Idx") )
Idx <- dataMaster$Idx
raw_dataMaster_numericalCol_names <- subset(config, 变量类型 == "Numerical")[, "变量名称"]
raw_dataMaster_categoricalCol_names <- subset(config, 变量类型 == "Categorical")[, "变量名称"]
raw_dataMaster_numerical <- cBind(Idx, dataMaster[, raw_dataMaster_numericalCol_names])
raw_dataMaster_categorical <- cBind(Idx, dataMaster[, raw_dataMaster_categoricalCol_names])
raw_dataMaster_categorical[] <- lapply(raw_dataMaster_categorical, function(x) str_trim(x))
# ================================================================================
saveRDS(dataMaster$Idx, "dataNew/Idx_dataMaster")
saveRDS(raw_dataMaster_numerical, "dataNew/num_dataMaster")
saveRDS(raw_dataMaster_categorical, "dataNew/cat_dataMaster")
# ================================================================================