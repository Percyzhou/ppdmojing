# 读取dataLog数据 -----------------------------------------------------------------
dataLog1_1 <- read.csv("data/Train/trainLog.csv", fileEncoding = "gbk", stringsAsFactors = F)
dataLog1_2 <- read.csv("data/Test_A/a_testLog1.csv", fileEncoding = "gbk", stringsAsFactors = F)
dataLog1_3 <- read.csv("data/Test_A/a_testLog2.csv", fileEncoding = "gbk", stringsAsFactors = F)
dataLog1 <- rBind(dataLog1_1, dataLog1_2, dataLog1_3)
dataLog2 <- read.csv("data/Test_B/b_testLog.csv", fileEncoding = "gbk", stringsAsFactors = F)

train_Idx <- dataMaster1$Idx
test_Idx <- dataMaster2$Idx
dataLog <- rBind(dataLog1, dataLog2)
# =================================================================================
# 提取"Idx"、"LogInfo1"、"LogInfo2"、"LogInfo3"子变量空间 ---------
dataLog_Idx <- dataLog[, "Idx", drop = T]
dataLog_LogInfo1 <- dataLog[, "LogInfo1", drop = F]
dataLog_LogInfo2 <- dataLog[, "LogInfo2", drop = F]
dataLog_LogInfo3 <- dataLog[, "LogInfo3", drop = F]
# =================================================================================
# 分别对各子集进行处理 ------------------------------------------------------------
dataLog_LogInfo1 <- one_hot(unlist(dataLog_LogInfo1))
dataLog_LogInfo1[dataLog_LogInfo1 == 0] <- NA
names(dataLog_LogInfo1) <- paste0("dataLog_logInfo1_", names(dataLog_LogInfo1))
dataLog_LogInfo2 <- one_hot(unlist(dataLog_LogInfo2))
dataLog_LogInfo2[dataLog_LogInfo2 == 0] <- NA
names(dataLog_LogInfo2) <- paste0("dataLog_logInfo1_", names(dataLog_LogInfo2))
dataLog_LogInfo3 <- data.frame(dataLog_LogInfo3 = as.numeric(as.Date(dataLog_LogInfo3$LogInfo3, "%Y-%m-%d")))
# =================================================================================
# 分别生成各种信息 ----------------------------------------------------------------
# 客户修改次数 --------------------------------------------------------------------
dataLog_count_by_Times <- c(table(dataLog_Idx))
# 客户修改了多少批数据 ------------------------------------------------------------
dataLog_count_by_Days_table <- table(data.frame(dataLog_Idx, dataLog_LogInfo3))
dataLog_count_by_Days_table[dataLog_count_by_Days_table != 0] <- 1
dataLog_count_by_Days <- apply(dataLog_count_by_Days_table, MARGIN = 1, sum)
# 客户在各类别中修改了多少次 ------------------------------------------------------
names_dataLog_LogInfo1 <- names(dataLog_LogInfo1)
dataLog_LogInfo1 <- lapply(dataLog_LogInfo1, function(x){
  data <- data.frame(dataLog_Idx, x)
  data.frame(table(data))[[3]]
})
dataLog_LogInfo1 <- do.call("cBind", dataLog_LogInfo1)
dataLog_LogInfo1 <- data.frame(dataLog_LogInfo1, stringsAsFactors = F)
names(dataLog_LogInfo1) <- names_dataLog_LogInfo1
# 客户在各类别中修改了多少次 ------------------------------------------------------
names_dataLog_LogInfo2 <- names(dataLog_LogInfo2)
dataLog_LogInfo2 <- lapply(dataLog_LogInfo2, function(x){
  data <- data.frame(dataLog_Idx, x)
  data.frame(table(data))[[3]]
})
dataLog_LogInfo2 <- do.call("cBind", dataLog_LogInfo2)
dataLog_LogInfo2 <- data.frame(dataLog_LogInfo2, stringsAsFactors = F)
names(dataLog_LogInfo2) <- names_dataLog_LogInfo2
# =================================================================================
# 合并相同Idx的样本 ---------------------------------------------------------------
dataLog_Idx <- unique(dataLog_Idx[order(dataLog_Idx)])
dataLog <- cBind(Idx = dataLog_Idx, dataLog_count_by_Times = dataLog_count_by_Times, 
                 dataLog_count_by_Days = dataLog_count_by_Days, dataLog_LogInfo1, dataLog_LogInfo2, stringsAsFactors = F)
saveRDS(dataLog, "dataNew/dataLog")
dataLog <- readRDS("dataNew/dataLog")
# =================================================================================
# 提取train和test的LogInfo数据 ----------------------------------------------------
dataLog_Train <- subset(dataLog, Idx %in% train_Idx)
dataLog_Test <- subset(dataLog, Idx %in% test_Idx)
# =================================================================================
