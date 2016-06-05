# 提取ListingInfo变量 ---------------------------------------------------
num_dataMaster <- readRDS("dataNew/num_dataMaster")
dataMaster_ListingInfo <- num_dataMaster[, grepl("^ListingInfo", names(num_dataMaster)), drop = F]
# =======================================================================
# 将日期数据转换为时间戳 ------------------------------------------------
dataMaster_ListingInfo[1:30000, 1] <-  as.numeric(as.Date(dataMaster_ListingInfo[1:30000, 1], format = "%Y/%m/%d"))
dataMaster_ListingInfo[30001:nrow(dataMaster_ListingInfo), 1] <- as.numeric(as.Date(dataMaster_ListingInfo[30001:nrow(dataMaster_ListingInfo), 1], format = "%d/%m/%Y"))
# =======================================================================