# 读取WeblogInfo数据 ---------------------------------------------------------
num_dataMaster <- readRDS("dataNew/num_dataMaster")
cat_dataMaster <- readRDS("dataNew/cat_dataMaster")

num_WeblogInfo <- num_dataMaster[, grepl("^WeblogInfo_", names(num_dataMaster))]
cat_WeblogInfo <- cat_dataMaster[, grepl("^WeblogInfo_", names(cat_dataMaster))]
# ============================================================================
# 处理WeblogInfo数据的数值变量部分 -------------------------------------------
num_WeblogInfo1 <- num_WeblogInfo[, vapply(c(1:7, 14:18), function(x) paste0("WeblogInfo_", x), character(1))]
num_WeblogInfo2 <- num_WeblogInfo[, vapply(c(8:13), function(x) paste0("WeblogInfo_", x), character(1))]
num_WeblogInfo3 <- num_WeblogInfo[, vapply(c(23:58), function(x) paste0("WeblogInfo_", x), character(1))]
# 暂不处理num_WeblogInfo1数据 ------------------------------------------------
num_WeblogInfo1[num_WeblogInfo1 == 0] <- NA
# ============================================================================
# 进行横向统计num_WeblogInfo2数据 --------------------------------------------
num_WeblogInfo2[num_WeblogInfo2 == 0] <- NA
num_WeblogInfo2_min <- analytics_byRow(num_WeblogInfo2, f = function(x) ifelse(all(is.na(x)), NA, min(x, na.rm = T)))
names(num_WeblogInfo2_min) <- "WeblogInfo_2nd_min"
num_WeblogInfo2_max <- analytics_byRow(num_WeblogInfo2, f = function(x) ifelse(all(is.na(x)), NA, max(x, na.rm = T)))
names(num_WeblogInfo2_max) <- "WeblogInfo_2nd_max"
num_WeblogInfo2_sum <- analytics_byRow(num_WeblogInfo2, f = function(x) ifelse(all(is.na(x)), NA, sum(x, na.rm = T)))
names(num_WeblogInfo2_sum) <- "WeblogInfo_2nd_sum"
num_WeblogInfo2_notEmpty <- analytics_byRow(num_WeblogInfo2, f = function(x) sum(!is.na(x), na.rm = T))
names(num_WeblogInfo2_notEmpty) <- "WeblogInfo_2nd_notEmpty"
num_WeblogInfo2 <- cBind(num_WeblogInfo2, num_WeblogInfo2_min, num_WeblogInfo2_max, num_WeblogInfo2_sum, num_WeblogInfo2_notEmpty)
# ============================================================================
# num_WeblogInfo3 进行横向统计 ------------------------------------
num_WeblogInfo3[num_WeblogInfo3 == 0] <- NA
num_WeblogInfo3_min <- analytics_byRow(num_WeblogInfo3, f = function(x) ifelse(all(is.na(x)), NA, min(x, na.rm = T)))
names(num_WeblogInfo3_min) <- "WeblogInfo_3rd_min"
num_WeblogInfo3_max <- analytics_byRow(num_WeblogInfo3, f = function(x) ifelse(all(is.na(x)), NA, max(x, na.rm = T)))
names(num_WeblogInfo3_max) <- "WeblogInfo_3rd_max"
num_WeblogInfo3_sum <- analytics_byRow(num_WeblogInfo3, f = function(x) ifelse(all(is.na(x)), NA, sum(x, na.rm = T)))
names(num_WeblogInfo3_sum) <- "WeblogInfo_3rd_sum"
num_WeblogInfo3_notEmpty <- analytics_byRow(num_WeblogInfo3, f = function(x) sum(!is.na(x), na.rm = T))
names(num_WeblogInfo3_notEmpty) <- "WeblogInfo_3rd_notEmpty"
num_WeblogInfo3 <- cBind(num_WeblogInfo3, num_WeblogInfo3_min, num_WeblogInfo3_max, num_WeblogInfo3_sum, num_WeblogInfo3_notEmpty)
# 组合WeblogInfo数据的数值变量------------------------------------------------
num_WeblogInfo <- cBind(num_WeblogInfo1, num_WeblogInfo2, num_WeblogInfo3)
# ============================================================================
# 对cat_WeblogInfo进行one hot 以及 one count 处理 -------------
cat_WeblogInfo[cat_WeblogInfo == ""] <- NA
cat_WeblogInfo1 <- cat_WeblogInfo[, vapply(c(19, 21), function(x) paste0("WeblogInfo_", x), character(1))]
cat_WeblogInfo1 <- lapply(cat_WeblogInfo1, FUN = one_hot)
cat_WeblogInfo1 <- do.call("cBind", cat_WeblogInfo1)
names(cat_WeblogInfo1) <- paste0("WeblogInfo_onehot_", names(cat_WeblogInfo1))
cat_WeblogInfo2 <- cat_WeblogInfo[, "WeblogInfo_20", drop = F]
cat_WeblogInfo2 <- lapply(cat_WeblogInfo2, one_count)
cat_WeblogInfo2 <- do.call("cBind", cat_WeblogInfo2)
cat_WeblogInfo3 <- mark_sparseValue(cat_WeblogInfo)
cat_WeblogInfo <- cBind(cat_WeblogInfo1, cat_WeblogInfo2, cat_WeblogInfo3)
# ============================================================================
# 组合WeblogInfo数据 ---------------------------------------------------------
dataMaster_WeblogInfo <- cBind(num_WeblogInfo, cat_WeblogInfo)
# ============================================================================