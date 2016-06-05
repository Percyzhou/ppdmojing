# 读取数据 -------------------------------------------------------------
Idx <- readRDS("dataNew/Idx_dataMaster")
num_dataMaster <- readRDS("dataNew/num_dataMaster")
cat_dataMaster <- readRDS("dataNew/cat_dataMaster")
# ======================================================================
# UserInfo数据 ---------------------------------------------------------
# 处理UserInfo中的类别变量 -------------------------------------------------
# 读取各省份经济信息 -------------------------------------------------------
pvn <- read.csv("config/province mac datas.csv", fileEncoding = "gbk", stringsAsFactors = F)
pvn$省 <- vapply(pvn$省, function(x) str_sub(str_trim(x), start = 1, end = 2), character(1))
# ==========================================================================
# 读取各城市经济发展信息 ---------------------------------------------------
# city <- read.xlsx("config/城市发展程度表old.xlsx", sheetIndex = 1, header = T, as.data.frame = T, stringsAsFactors = F)
# ==========================================================================
# 提取出所有的UserInfo特征 -------------------------------------------------
cat_UserInfo <- cat_dataMaster[, grepl("^UserInfo_", names(cat_dataMaster))]
# ==========================================================================
# 替换空值 -----------------------------------------------------------------
cat_UserInfo[is.na(cat_UserInfo)] <- NA
# cat_UserInfo[cat_UserInfo == ""] <- NA
# cat_UserInfo[cat_UserInfo == "不详"] <- NA
# cat_UserInfo[cat_UserInfo == "D"] <- NA
# ==========================================================================
# 用户地理信息 -------------------------------------------------------------
# 处理省份变量 -------------------------------------------------------------
provienceInfo <- cat_UserInfo[, c("UserInfo_7", "UserInfo_19")]
provienceInfo[] <- lapply(provienceInfo, function(x) str_sub(str_trim(x), start = 1, end = 2))
# provienceInfo[] <- lapply(provienceInfo, function(x) arrangeChar(x, charLists = list(c("青海", "新疆", "西藏"))))
provienceInfo_onehot <- lapply(provienceInfo, one_hot)
provienceInfo_onehot <- do.call("cBind", provienceInfo_onehot)
names(provienceInfo_onehot) <- paste0("provienceInfo_", names(provienceInfo_onehot))
# provienceInfo_count <- count_theCategory(data = provienceInfo)
# provienceInfo_count <- lapply(provienceInfo_count, function(x) rank(x, na.last = "keep"))
# provienceInfo_count <- data.frame(provienceInfo_count, stringsAsFactors = F)
# provienceInfo_stay1 <- provienceInfo[, "UserInfo_7", drop = F]
# provienceInfo_stay2 <- provienceInfo[, "UserInfo_19", drop = F]
# provienceInfo_economic1 <- lapply(provienceInfo_stay1[[1]], function(x) unlist(ifelse(is.na(x) | x == "不详" | x == "N1" | x == "N2", list(matrix(NA, nrow = 1, ncol = 11)), list(subset(pvn, 省 == x)[, c(2:8, 12)]))))
# provienceInfo_economic1 <- do.call("rBind", provienceInfo_economic1)
# provienceInfo_economic2 <- lapply(provienceInfo_stay2[[1]], function(x) unlist(ifelse(is.na(x) | x == "不详" | x == "N1" | x == "N2", list(matrix(NA, nrow = 1, ncol = 11)), list(subset(pvn, 省 == x)[, c(2:8, 12)]))))
# provienceInfo_economic2 <- do.call("rBind", provienceInfo_economic2)
# provienceInfo_economic3 <- (provienceInfo_economic1 - provienceInfo_economic2)[, "gdp", drop = F]
provienceInfo <- cBind(provienceInfo_onehot)
# 处理城市变量 -------------------------------------------------------------
cityInfo <- cat_UserInfo[, c("UserInfo_2", "UserInfo_4", "UserInfo_8", "UserInfo_20")]
cityInfo_class <- cityInfo
cityInfo <- lapply(cityInfo, one_hot)
cityInfo <- do.call("cBind", cityInfo)
names(cityInfo) <- paste0("cityInfo_", names(cityInfo))
# cityInfo_num <- analytics_byRow(data = cityInfo_raw, f = function(x) length(table(x)))
# cityInfo_max <- analytics_byRow(data = cityInfo_raw, f = function(x) ifelse(all(is.na(x)), 0, max(table(x), na.rm = T)))
cityInfo <- cBind(cityInfo)

cityInfo_lowHigh <- cat_UserInfo[, c("UserInfo_25", "UserInfo_26")]
cityInfo_lowHigh <- lapply(cityInfo_lowHigh, as.numeric)
names(cityInfo_lowHigh) <- c("cityInfo_lowtohigh", "cityInfo_hightolow")
# villageInfo[villageInfo == "d" | villageInfo == 28] <- 0
# villageInfo[villageInfo != 0] <- 1

# dataMaster_UserInfo_place_provience_woe1 <- map_woe(x = unlist(dataMaster_UserInfo_place_provience_stay1[1:length(target),]), y = unlist(dataMaster_UserInfo_place_provience_stay1), target = target, Threshold = 100, name = province_woe1)
# dataMaster_UserInfo_place_provience_woe2 <- map_woe(x = unlist(dataMaster_UserInfo_place_provience_stay2[1:length(target),]), y = unlist(dataMaster_UserInfo_place_provience_stay2), target = target, Threshold = 100, name = province_woe1)
# 组合全部用户地理信息 -----------------------------------------------------
positionInfo <- cBind(provienceInfo, cityInfo, cityInfo_lowHigh)
# ==========================================================================
# 对移动供应商、婚姻情况、学历信息进行one hot处理和count处理 ---------------
cat_UserInfo_other<- cat_UserInfo[, vapply(c(5, 6, 9, 11, 12, 13, 17, 21, 22, 23), function(x) paste0("UserInfo_", x), character(1))]
cat_UserInfo_other1 <- lapply(cat_UserInfo_other, FUN = one_hot)
cat_UserInfo_other1 <- do.call("cBind", cat_UserInfo_other1)
names(cat_UserInfo_other1) <- paste0("UserInfo_onehot_", names(cat_UserInfo_other1))
# cat_UserInfo_other2 <- count_theCategory(cat_UserInfo[, "UserInfo_23", drop = F])
cat_UserInfo_other <- cBind(cat_UserInfo_other1)
cat_UserInfo <- cBind(positionInfo, cat_UserInfo_other)
# ==========================================================================

# 处理UserInfo的数值变量 ---------------------------------------------------
num_UserInfo <- num_dataMaster[, grepl("^UserInfo_", names(num_dataMaster))]
# ==========================================================================
# 将所有UserInfo的变量组合起来 ---------------------------------------------
dataMaster_UserInfo <- cBind(cat_UserInfo, num_UserInfo)
# ==========================================================================
# ==========================================================================
# 提取教育信息 ------------------------------------------------------
cat_Education <- cat_dataMaster[, c("Education_Info1", "Education_Info5")]
# ===================================================================
# # 标记"E"和"不详"为空值
# cat_Education[cat_Education == "E"] <- NA
# cat_Education[cat_Education == "不详"] <- NA
# # ===================================================================
# 对dataEducation做one hot转换，并标记空值位置 ----------------------
cat_Education_onehot <- lapply(cat_Education, FUN = one_hot)
cat_Education_onehot <- do.call("cBind", cat_Education_onehot)
names(cat_Education_onehot) <- paste0("Education_onehot_", names(cat_Education_onehot))
cat_Education <- cBind(cat_Education_onehot)
# ===================================================================
# 处理ThirdParty数据 ----------------------------------------------------------
num_ThirdParty <- num_dataMaster[, grepl("^ThirdParty_Info_Period", names(num_dataMaster))]
num_ThirdParty_backup <- num_dataMaster[, grepl("^ThirdParty_Info_Period", names(num_dataMaster))]
# for(x in paste0("ThirdParty_Info_Period7_", 1:17)) num_ThirdParty[[x]] <- NULL
# num_ThirdParty_a <- num_ThirdParty
# num_ThirdParty_b <- num_ThirdParty
num_ThirdParty[num_ThirdParty == -1] <- NA
num_ThirdParty_backup[num_ThirdParty_backup == -1] <- -100
num_ThirdParty_backup[] <- lapply(num_ThirdParty_backup, function(x) as.numeric(cut(x, breaks = 100, labels = 1:100)))
# num_ThirdParty_a[num_ThirdParty_a == -1] <- NA
# num_ThirdParty_a[] <- lapply(num_ThirdParty_a, function(x){
#   x[is.na(x)] <- median(x, na.rm = T)
#   return(x)
# })
# =============================================================================
class_names1 <- vapply(1:12, function(x) paste0("_", x, "$"), character(1))
class_names2 <- vapply(13:17, function(x) paste0("_", x, "$"), character(1))
num_ThirdParty_list1 <- lapply(class_names1, function(x) num_ThirdParty[, grepl(x, names(num_ThirdParty))])
num_ThirdParty_list2 <- lapply(class_names2, function(x) num_ThirdParty[, grepl(x, names(num_ThirdParty))])
num_ThirdParty1 <- do.call("cBind", num_ThirdParty_list1)
num_ThirdParty1 <- lapply(num_ThirdParty1, function(x) x)
num_ThirdParty1 <- do.call("cBind", num_ThirdParty1)
num_ThirdParty1 <- data.frame(num_ThirdParty1, stringsAsFactors = F)
num_ThirdParty1[] <- lapply(num_ThirdParty1, function(x) as.numeric(cut(x, breaks = 100, labels = 1:100)))
num_ThirdParty2 <- do.call("cBind", num_ThirdParty_list2)
num_ThirdParty2 <- lapply(num_ThirdParty2, function(x) x)
num_ThirdParty2 <- do.call("cBind", num_ThirdParty2)
num_ThirdParty2 <- data.frame(num_ThirdParty2, stringsAsFactors = F)
num_ThirdParty2 <- lapply(num_ThirdParty2, function(x) as.numeric(cut(x, breaks = 100, labels = 1:100)))

# 对ThirdParty数据进行划分 ----------------------------------------------------
num_ThirdParty_list1_min <- lapply(num_ThirdParty_list1, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, min(x_row, na.rm = T))))
num_ThirdParty_list1_mean <- lapply(num_ThirdParty_list1, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, mean(x_row, na.rm = T))))
num_ThirdParty_list1_var <- lapply(num_ThirdParty_list1, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, var(x_row, na.rm = T))))

num_ThirdParty1_min <- do.call("cBind", num_ThirdParty_list1_min)
names(num_ThirdParty1_min) <- paste0("ThirdParty_Info_Period_any_", 1:12, "_min")
num_ThirdParty1_mean <- do.call("cBind", num_ThirdParty_list1_mean)
names(num_ThirdParty1_mean) <- paste0("ThirdParty_Info_Period_any_", 1:12, "_mean")
num_ThirdParty1_var <- do.call("cBind", num_ThirdParty_list1_var)
names(num_ThirdParty1_var) <- paste0("ThirdParty_Info_Period_any_", 1:12, "_var")

num_ThirdParty_list2_min <- lapply(num_ThirdParty_list2, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, min(x_row, na.rm = T))))
num_ThirdParty_list2_mean <- lapply(num_ThirdParty_list2, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, mean(x_row, na.rm = T))))
num_ThirdParty_list2_var <- lapply(num_ThirdParty_list2, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, var(x_row, na.rm = T))))

num_ThirdParty2_min <- do.call("cBind", num_ThirdParty_list2_min)
names(num_ThirdParty2_min) <- paste0("ThirdParty_Info_Period_any_", 13:17, "_min")
num_ThirdParty2_mean <- do.call("cBind", num_ThirdParty_list2_mean)
names(num_ThirdParty2_mean) <- paste0("ThirdParty_Info_Period_any_", 13:17, "_mean")
num_ThirdParty2_var <- do.call("cBind", num_ThirdParty_list2_var)
names(num_ThirdParty2_var) <- paste0("ThirdParty_Info_Period_any_", 13:17, "_var")
num_ThirdParty_withMonth <- cBind(num_ThirdParty1_min, num_ThirdParty1_mean, num_ThirdParty1_var, num_ThirdParty2_min, num_ThirdParty2_mean, num_ThirdParty2_var)

class_names3 <- lapply(1:7, function(x) paste0("ThirdParty_Info_Period", rep(x, 12), "_", 1:12))
class_names4 <- lapply(1:7, function(x) paste0("ThirdParty_Info_Period", rep(x, 5), "_", 13:17))
num_ThirdParty_list3 <- lapply(class_names3, function(x) num_ThirdParty[, names(num_ThirdParty) %in% x])
num_ThirdParty_list4 <- lapply(class_names4, function(x) num_ThirdParty[, names(num_ThirdParty) %in% x])

num_ThirdParty_1_list3 <- Map(f = function(x, y) statByTime(data = x, breaks = 1, Fun = c(min, mean, var, caculate_slide), name = paste0("ThirdParty_Info_Period_", y), nameFun = c("min", "mean", "var", "lm")), num_ThirdParty_list3, 1:7)
num_ThirdParty_2_list3 <- Map(f = function(x, y) statByTime(data = x, breaks = 2, Fun = c(min, mean, var, caculate_slide), name = paste0("ThirdParty_Info_Period_", y), nameFun = c("min", "mean", "var", "lm")), num_ThirdParty_list3, 1:7)
num_ThirdParty_4_list3 <- Map(f = function(x, y) statByTime(data = x, breaks = 4, Fun = c(min, mean, var, caculate_slide), name = paste0("ThirdParty_Info_Period_", y), nameFun = c("min", "mean", "var", "lm")), num_ThirdParty_list3, 1:7)
num_ThirdParty_all_list4 <- Map(f = function(x, y) statByTime(data = x, breaks = 1, Fun = c(mean, var, min, caculate_slide), name = paste0("ThirdParty_Info_Period_", y), nameFun = c("min", "mean", "var", "lm")), num_ThirdParty_list4, 1:7)
num_ThirdParty_withCategory1 <- do.call("cBind", c(num_ThirdParty_1_list3, num_ThirdParty_2_list3, num_ThirdParty_4_list3))
num_ThirdParty_withCategory2 <- do.call("cBind", num_ThirdParty_all_list4)
saveRDS(num_ThirdParty_withCategory1, "dataNew/num_ThirdParty_withCategory1")
saveRDS(num_ThirdParty_withCategory2, "dataNew/num_ThirdParty_withCategory2")
num_ThirdParty_withCategory1 <- readRDS("dataNew/num_ThirdParty_withCategory1")
num_ThirdParty_withCategory2 <- readRDS("dataNew/num_ThirdParty_withCategory2")
col_flag1 <- grepl("1_1", names(num_ThirdParty_withCategory1))
col_flag2 <- grepl("4_4", names(num_ThirdParty_withCategory1))
num_ThirdParty_withCategory_new <- cBind(num_ThirdParty_withCategory1[, col_flag1 | col_flag2], num_ThirdParty_withCategory2)
num_ThirdParty_withCategory_new[] <- lapply(num_ThirdParty_withCategory_new, function(x) as.numeric(cut(x, breaks = 100, labels = 1:100)))

num_ThirdParty5 <- num_ThirdParty1
num_ThirdParty5_1 <- lapply(num_ThirdParty5, function(x) as.numeric(cut(x, breaks = 100, labels = 1:100)))
num_ThirdParty5_1 <- data.frame(num_ThirdParty5_1, stringsAsFactors = F)
names(num_ThirdParty5_1) <- paste0(names(num_ThirdParty5_1), "_rank")
num_ThirdParty5_2 <- lapply(seq(from = 0.4, to = 0.6, by = 0.1), function(x) analytics_byRow(num_ThirdParty5_1, f = function(x_row) quantile(x_row, probs = x, na.rm = T)))
num_ThirdParty5_2 <- do.call("cBind", num_ThirdParty5_2)
names(num_ThirdParty5_2) <- paste0("ThirdParty_1st_rankSummary_", seq(from = 0.4, to = 0.6, by = 0.1))
num_ThirdParty5_3 <- lapply(c(-1, 0), function(x) analytics_byRow(num_ThirdParty_list1, f = function(x_row) sum(x_row == x, na.rm = NA)))
num_ThirdParty5_3 <- do.call("cBind", num_ThirdParty5_3)
names(num_ThirdParty5_3) <- c("ThirdParty_1st_is-1", "ThirdParty_1st_is0")
num_ThirdParty5 <- cBind(num_ThirdParty5_2)

num_ThirdParty6 <- num_ThirdParty2
num_ThirdParty6_1 <- lapply(num_ThirdParty6, function(x) as.numeric(cut(x, breaks = 100, labels = 1:100)))
num_ThirdParty6_1 <- data.frame(num_ThirdParty6_1, stringsAsFactors = F)
names(num_ThirdParty6_1) <- paste0(names(num_ThirdParty6_1), "_rank")
num_ThirdParty6_2 <- lapply(c(0.4, 0.7, 0.9, 1), function(x) analytics_byRow(num_ThirdParty6_1, f = function(x_row) quantile(x_row, probs = x, na.rm = T)))
num_ThirdParty6_2 <- do.call("cBind", num_ThirdParty6_2)
names(num_ThirdParty6_2) <- paste0("ThirdParty_2nd_rankSummary_", c(0.4, 0.7, 0.9, 1))
num_ThirdParty6_3 <- lapply(c(-1, 0), function(x) analytics_byRow(num_ThirdParty_list2, f = function(x_row) sum(x_row == x, na.rm = NA)))
num_ThirdParty6_3 <- do.call("cBind", num_ThirdParty6_3)
names(num_ThirdParty6_3) <- c("ThirdParty_2nd_is-1", "ThirdParty_2nd_is0z")
num_ThirdParty6 <- cBind(num_ThirdParty6_2)

num_ThirdParty <- cBind(num_ThirdParty_backup, num_ThirdParty_withMonth, num_ThirdParty_withCategory_new, num_ThirdParty5, num_ThirdParty6)
# =============================================================================
# 读取WeblogInfo数据 ---------------------------------------------------------
num_WeblogInfo <- num_dataMaster[, grepl("^WeblogInfo_", names(num_dataMaster))]
cat_WeblogInfo <- cat_dataMaster[, grepl("^WeblogInfo_", names(cat_dataMaster))]
# ============================================================================
# 处理WeblogInfo数据的数值变量部分 -------------------------------------------
num_WeblogInfo1 <- num_WeblogInfo[, vapply(c(2, 4:7, 14:18), function(x) paste0("WeblogInfo_", x), character(1))]
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
cat_WeblogInfo1 <- cat_WeblogInfo[, c("WeblogInfo_19", "WeblogInfo_20", "WeblogInfo_21")]
cat_WeblogInfo1 <- lapply(cat_WeblogInfo1, FUN = one_hot)
cat_WeblogInfo1 <- do.call("cBind", cat_WeblogInfo1)
names(cat_WeblogInfo1) <- paste0("WeblogInfo_onehot_", names(cat_WeblogInfo1))
# cat_WeblogInfo2 <- cat_WeblogInfo[, "WeblogInfo_20", drop = F]
# cat_WeblogInfo2 <- lapply(cat_WeblogInfo2, one_count)
# cat_WeblogInfo2 <- do.call("cBind", cat_WeblogInfo2)
cat_WeblogInfo3 <- mark_sparseValue(cat_WeblogInfo)
cat_WeblogInfo <- cBind(cat_WeblogInfo1)
# ============================================================================
# 组合WeblogInfo数据 ---------------------------------------------------------
dataMaster_WeblogInfo <- cBind(num_WeblogInfo, cat_WeblogInfo)
# ============================================================================
# 提取SocialNetwork子集 -----------------------------------------------------
num_SocialNetwork <- num_dataMaster[, grepl("^SocialNetwork", names(num_dataMaster))]
cat_SocialNetwork <- cat_dataMaster[, grepl("^SocialNetwork", names(cat_dataMaster))]
# ===========================================================================
# 处理SocialNetwork数据的数值变量 -------------------------------------------
num_SocialNetwork[num_SocialNetwork == -1] <- NA
num_SocialNetwork_min <- analytics_byRow(num_SocialNetwork, f = function(x) ifelse(all(is.na(x)), NA, min(x, na.rm = T)))
names(num_SocialNetwork_min) <- "SocialNetwork_min"
num_SocialNetwork_max <- analytics_byRow(num_SocialNetwork, f = function(x) ifelse(all(is.na(x)), NA, max(x, na.rm = T)))
names(num_SocialNetwork_max) <- "SocialNetwork_max"
num_SocialNetwork_sum <- analytics_byRow(num_SocialNetwork, f = function(x) ifelse(all(is.na(x)), NA, sum(x, na.rm = T)))
names(num_SocialNetwork_sum) <- "SocialNetwork_sum"
num_SocialNetwork_sparseRate <- analytics_byRow(num_SocialNetwork, f = function(x) ifelse(all(is.na(x)), NA, sum(is.na(x))))
names(num_SocialNetwork_sparseRate) <- "SocialNetwork_sparseRate"

num_SocialNetwork <- cBind(num_SocialNetwork, num_SocialNetwork_min, num_SocialNetwork_max, num_SocialNetwork_sum)
# ===========================================================================
# 提取多种衍生变量 ----------------------------------------------------------
cat_SocialNetwork <- lapply(cat_SocialNetwork, one_hot)
cat_SocialNetwork <- do.call("cBind", cat_SocialNetwork)
names(cat_SocialNetwork) <- paste0("SocialNetwork_onehot_", names(cat_SocialNetwork))
# ===========================================================================
# 组合SocialNetwork数据 -----------------------------------------------------
dataMaster_SocialNetwork <- cBind(num_SocialNetwork, cat_SocialNetwork)
# ===========================================================================
# 提取ListingInfo变量 ---------------------------------------------------
dataMaster_ListingInfo <- num_dataMaster[, grepl("^ListingInfo", names(num_dataMaster)), drop = F]
# =======================================================================
# 将日期数据转换为时间戳 ------------------------------------------------
dataMaster_ListingInfo1 <- dataMaster_ListingInfo
dataMaster_ListingInfo2 <- dataMaster_ListingInfo
dataMaster_ListingInfo1[1:60000, 1] <-  as.numeric(as.Date(dataMaster_ListingInfo1[1:60000, 1], format = "%Y-%m-%d"))
dataMaster_ListingInfo1[60001:79999, 1] <- as.numeric(as.Date(dataMaster_ListingInfo1[60001:79999, 1], format = "%Y/%m/%d"))
dataMaster_ListingInfo1[80000:89999, 1] <- as.numeric(as.Date(dataMaster_ListingInfo1[80000:89999, 1], format = "%d/%m/%Y"))
dataMaster_ListingInfo1$ListingInfo <- as.numeric(cut(dataMaster_ListingInfo1$ListingInfo, breaks = 100, labels = 1:100))
# dataMaster_ListingInfo2[1:60000, 1] <-  weekdays(as.Date(dataMaster_ListingInfo1[1:60000, 1], format = "%Y-%m-%d"))
# dataMaster_ListingInfo2[60001:70000, 1] <- weekdays(as.Date(dataMaster_ListingInfo1[60001:70000, 1], format = "%d/%m/%Y"))
# dataMaster_ListingInfo_onehot <- lapply(dataMaster_ListingInfo2, one_hot)
# dataMaster_ListingInfo_onehot <- do.call("cBind", dataMaster_ListingInfo_onehot)
# names(dataMaster_ListingInfo_onehot) <- paste0("ListingInfo_onehot_", names(dataMaster_ListingInfo_onehot))
dataMaster_ListingInfo <- cBind(dataMaster_ListingInfo1)
# =======================================================================
# 提取转换数据 ----------------------------------------------------------
num_catToNum <- num_dataMaster[, c("UserInfo_1", "UserInfo_3", "UserInfo_14", "UserInfo_15", "UserInfo_16")]
num_provienceInfo <- num_dataMaster[, grepl("^provienceEconomicInfo_", names(num_dataMaster))]
# =======================================================================
# 组合全部数据 ----------------------------------------------------------
dataMaster_specialNewXs <- cBind(Idx = Idx, dataMaster_UserInfo, cat_Education, num_ThirdParty, dataMaster_WeblogInfo, dataMaster_SocialNetwork, dataMaster_ListingInfo, num_catToNum, num_provienceInfo)
dataMaster_Train <- subset(dataMaster_specialNewXs, Idx %in% train_Idx)
dataMaster_Test <- subset(dataMaster_specialNewXs, Idx %in% test_Idx)
# =======================================================================
# =======================================================================