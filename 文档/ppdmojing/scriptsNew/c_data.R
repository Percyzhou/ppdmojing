# dataMaster ------------------------------------------------
trainMaster1 <- read.csv("data/Train/PPD_Training_Master_GBK_3_1_Training_Set.csv", fileEncoding = 'gbk', header = T, stringsAsFactors = F)
trainMaster2 <- read.csv("data/Train/Kesci_Master_9w_gbk_3_2.csv", fileEncoding = 'gbk', header = T, stringsAsFactors = F)
trainMaster <- rBind(trainMaster1, trainMaster2)
trainMaster <- trainMaster[order(trainMaster$Idx),]
write.csv(trainMaster, "data/Train/trainMaster.csv", row.names = F, fileEncoding = 'gbk')

a_testMaster_part1 <- read.csv("data/Test_A/Kesci_Master_9w_gbk_2.csv", fileEncoding = 'gbk', header = T, stringsAsFactors = F)
# a_testMaster_part1$target <- NULL
a_testMaster_part2_1 <- read.csv("data/Test_A/daily_test.csv", fileEncoding = 'gbk', stringsAsFactors = F)[, "Idx", drop = F]
a_testMaster_part2_2 <- read.csv("data/Test_A/final_test.csv", fileEncoding = 'gbk', stringsAsFactors = F)[, "Idx", drop = F]
a_testMaster1 <- merge(a_testMaster_part1, a_testMaster_part2_1, by = 'Idx', all = F)
a_testMaster2 <- merge(a_testMaster_part1, a_testMaster_part2_2, by = 'Idx', all = F)
write.csv(a_testMaster1, "data/Test_A/a_testMaster1.csv", row.names = F, fileEncoding = 'gbk')
write.csv(a_testMaster2, "data/Test_A/a_testMaster2.csv", row.names = F, fileEncoding = 'gbk')

b_testMaster <- read.csv("data/Test_B/Kesci_Master_9w_gbk_1_test_set.csv", fileEncoding = 'gbk', header = T, stringsAsFactors = F)
write.csv(b_testMaster, "data/Test_B/b_testMaster.csv", row.names = F, fileEncoding = 'gbk')
# ===========================================================
# dataLog ---------------------------------------------------
trainLog1 <- read.csv("data/Train/PPD_LogInfo_3_1_Training_Set.csv", fileEncoding = 'gbk', header = T, stringsAsFactors = F)
trainLog2 <- read.csv("data/Train/LogInfo_9w_3_2.csv", fileEncoding = 'gbk', header = T, stringsAsFactors = F)
trainLog <- rBind(trainLog1, trainLog2)
write.csv(trainLog, "data/Train/trainLog.csv", row.names = F, fileEncoding = 'gbk')

a_testLog <- read.csv("data/Test_A/LogInfo_9w_2.csv", fileEncoding = 'gbk', header = T, stringsAsFactors = F)
a_testLog1 <- subset(a_testLog, Idx %in% a_testMaster1$Idx)
a_testLog2 <- subset(a_testLog, Idx %in% a_testMaster2$Idx)
write.csv(a_testLog1, "data/Test_A/a_testLog1.csv", row.names = F, fileEncoding = 'gbk')
write.csv(a_testLog2, "data/Test_A/a_testLog2.csv", row.names = F, fileEncoding = 'gbk')

b_testLog <- read.csv("data/Test_B/LogInfo_9w_1.csv", fileEncoding = 'gbk', header = T, stringsAsFactors = F)
write.csv(b_testLog, "data/Test_B/b_testLog.csv", row.names = F, fileEncoding = 'gbk')
# ===========================================================
# dataUserupdate --------------------------------------------
trainUserupdate1 <- read.csv("data/Train/PPD_Userupdate_Info_3_1_Training_Set.csv", fileEncoding = 'gbk', header = T, stringsAsFactors = F)
trainUserupdate2 <- read.csv("data/Train/Userupdate_Info_9w_3_2.csv", fileEncoding = 'gbk', header = T, stringsAsFactors = F)
trainUserupdate <- rBind(trainUserupdate1, trainUserupdate2)
write.csv(trainUserupdate, "data/Train/trainUserupdate.csv", row.names = F, fileEncoding = 'gbk')

a_testUserupdate <- read.csv("data/Test_A/Userupdate_Info_9w_2.csv", fileEncoding = 'gbk', header = T, stringsAsFactors = F)
a_testUserupdate1 <- subset(a_testUserupdate, Idx %in% a_testMaster1$Idx)
a_testUserupdate2 <- subset(a_testUserupdate, Idx %in% a_testMaster2$Idx)
write.csv(a_testUserupdate1, "data/Test_A/a_testUserupdate1.csv", row.names = F, fileEncoding = 'gbk')
write.csv(a_testUserupdate2, "data/Test_A/a_testUserupdate2.csv", row.names = F, fileEncoding = 'gbk')

b_testUserupdate <- read.csv("data/Test_B/Userupdate_Info_9w_1.csv", fileEncoding = 'gbk', header = T, stringsAsFactors = F)
write.csv(b_testUserupdate, "data/Test_B/b_testUserupdate.csv", row.names = F, fileEncoding = 'gbk')
# ===========================================================