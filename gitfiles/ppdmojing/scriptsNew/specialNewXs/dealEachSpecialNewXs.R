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
provienceInfo_count <- count_theCategory(data = provienceInfo)
provienceInfo_count <- lapply(provienceInfo_count, function(x) rank(x, na.last = "keep"))
provienceInfo_count <- data.frame(provienceInfo_count, stringsAsFactors = F)
provienceInfo_stay1 <- provienceInfo[, "UserInfo_7", drop = F]
provienceInfo_stay2 <- provienceInfo[, "UserInfo_19", drop = F]
provienceInfo_economic1 <- lapply(provienceInfo_stay1[[1]], function(x) unlist(ifelse(is.na(x) | x == "不详" | x == "N1" | x == "N2", list(matrix(NA, nrow = 1, ncol = 11)), list(subset(pvn, 省 == x)[, 2:12]))))
provienceInfo_economic1 <- do.call("rBind", provienceInfo_economic1)
provienceInfo_economic2 <- lapply(provienceInfo_stay2[[1]], function(x) unlist(ifelse(is.na(x) | x == "不详" | x == "N1" | x == "N2", list(matrix(NA, nrow = 1, ncol = 11)), list(subset(pvn, 省 == x)[, 2:12]))))
provienceInfo_economic2 <- do.call("rBind", provienceInfo_economic2)
provienceInfo_economic3 <- (provienceInfo_economic1 - provienceInfo_economic2)[, "gdp", drop = F]
provienceInfo <- cBind(provienceInfo_onehot, provienceInfo_count, provienceInfo_economic1, provienceInfo_economic2, provienceInfo_economic3)
# 处理城市变量 -------------------------------------------------------------
cityInfo <- cat_UserInfo[, c("UserInfo_8", "UserInfo_20")]
cityInfo_class <- cityInfo
cityInfo <- lapply(cityInfo, one_hot)
cityInfo <- do.call("cBind", cityInfo)
names(cityInfo) <- paste0("cityInfo_", names(cityInfo))
# cityInfo_class[] <- lapply(cityInfo_class, function(x) str_sub(str_trim(x), start = 1, end = 4))
# cityInfo_class <- lapply(cityInfo_class, one_hot)
# cityInfo_class <- do.call("cBind", cityInfo)
# names(cityInfo_class) <- paste0("cityInfo_", names(cityInfo_class))
cityInfo_num <- analytics_byRow(data = cityInfo_raw, f = function(x) length(table(x)))
cityInfo_max <- analytics_byRow(data = cityInfo_raw, f = function(x) ifelse(all(is.na(x)), 0, max(table(x), na.rm = T)))
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
cat_UserInfo_other3 <- mark_sparseValue(cat_UserInfo_other)
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
cat_Education <- cat_dataMaster[, grepl("^Education", names(cat_dataMaster))]
# ===================================================================
# # 标记"E"和"不详"为空值
# cat_Education[cat_Education == "E"] <- NA
# cat_Education[cat_Education == "不详"] <- NA
# # ===================================================================
# 对dataEducation做one hot转换，并标记空值位置 ----------------------
cat_Education_onehot <- lapply(cat_Education, FUN = one_hot)
cat_Education_onehot <- do.call("cBind", cat_Education_onehot)
names(cat_Education_onehot) <- paste0("Education_onehot_", names(cat_Education_onehot))
cat_Education_sparse <- mark_sparseValue(data = cat_Education)
names(cat_Education_sparse) <- paste0("Education_sparse_", names(cat_Education_sparse))
cat_Education <- cBind(cat_Education_onehot)
# ===================================================================
# 处理ThirdParty数据 ----------------------------------------------------------
num_ThirdParty <- num_dataMaster[, grepl("^ThirdParty_Info_Period", names(num_dataMaster))]
num_ThirdParty_a <- num_ThirdParty
num_ThirdParty_b <- num_ThirdParty
num_ThirdParty[num_ThirdParty == -1] <- NA
num_ThirdParty_a[num_ThirdParty_a == -1] <- NA
num_ThirdParty_a[] <- lapply(num_ThirdParty_a, function(x){
  x[is.na(x)] <- median(x, na.rm = T)
  return(x)
})
# num_ThirdParty_b[num_ThirdParty_b == -1] <- -200
# num_ThirdParty_b[num_ThirdParty_b == 0] <- -100
# =============================================================================
class_names1 <- vapply(1:12, function(x) paste0("_", x, "$"), character(1))
class_names2 <- vapply(13:17, function(x) paste0("_", x, "$"), character(1))
num_ThirdParty_list1 <- lapply(class_names1, function(x) num_ThirdParty[, grepl(x, names(num_ThirdParty))])
num_ThirdParty_list2 <- lapply(class_names2, function(x) num_ThirdParty[, grepl(x, names(num_ThirdParty))])
num_ThirdParty1 <- do.call("cBind", num_ThirdParty_list1)
num_ThirdParty1 <- lapply(num_ThirdParty1, function(x) x)
num_ThirdParty1 <- do.call("cBind", num_ThirdParty1)
num_ThirdParty1 <- data.frame(num_ThirdParty1, stringsAsFactors = F)
num_ThirdParty1[] <- lapply(num_ThirdParty1, function(x) as.numeric(cut(x, breaks = 600, labels = 1:600)))
num_ThirdParty2 <- do.call("cBind", num_ThirdParty_list2)
num_ThirdParty2 <- lapply(num_ThirdParty2, function(x) x)
num_ThirdParty2 <- do.call("cBind", num_ThirdParty2)
num_ThirdParty2 <- data.frame(num_ThirdParty2, stringsAsFactors = F)
num_ThirdParty2 <- lapply(num_ThirdParty2, function(x) as.numeric(cut(x, breaks = 500, labels = 1:500)))
# ================================================================================================
# 对ThirdParty数据进行划分 ----------------------------------------------------
num_ThirdParty_list1_min <- lapply(num_ThirdParty_list1, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, min(x_row, na.rm = T))))
num_ThirdParty_list1_max <- lapply(num_ThirdParty_list1, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, max(x_row, na.rm = T))))
num_ThirdParty_list1_sum <- lapply(num_ThirdParty_list1, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, sum(x_row, na.rm = T))))
num_ThirdParty_list1_mean <- lapply(num_ThirdParty_list1, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, mean(x_row, na.rm = T))))
num_ThirdParty_list1_sd <- lapply(num_ThirdParty_list1, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, sd(x_row, na.rm = T))))
num_ThirdParty_list1_meanVsd <- lapply(num_ThirdParty_list1, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, (mean(x_row, na.rm = T) + 1) / (sd(x_row, na.rm = T) + 1))))

num_ThirdParty1_min <- do.call("cBind", num_ThirdParty_list1_min)
names(num_ThirdParty1_min) <- paste0("ThirdParty_Info_Period_any_", 1:12, "_min")
num_ThirdParty1_max <- do.call("cBind", num_ThirdParty_list1_max)
names(num_ThirdParty1_max) <- paste0("ThirdParty_Info_Period_any_", 1:12, "_max")
num_ThirdParty1_sum <- do.call("cBind", num_ThirdParty_list1_sum)
names(num_ThirdParty1_sum) <- paste0("ThirdParty_Info_Period_any_", 1:12, "_sum")
num_ThirdParty1_mean <- do.call("cBind", num_ThirdParty_list1_mean)
names(num_ThirdParty1_mean) <- paste0("ThirdParty_Info_Period_any_", 1:12, "_mean")
num_ThirdParty1_sd <- do.call("cBind", num_ThirdParty_list1_sd)
names(num_ThirdParty1_sd) <- paste0("ThirdParty_Info_Period_any_", 1:12, "_sd")
num_ThirdParty1_meanVsd <- do.call("cBind", num_ThirdParty_list1_meanVsd)
names(num_ThirdParty1_meanVsd) <- paste0("ThirdParty_Info_Period_any_", 1:12, "_meanVsd")
# num_ThirdParty1_minAll <- analytics_byRow(num_ThirdParty1_min, f = function(x_row) ifelse(all(is.na(x_row)), NA, min(x_row, na.rm = T)))
# names(num_ThirdParty1_minAll) <- "ThirdParty_1st_minAll"
# num_ThirdParty1_maxAll <- analytics_byRow(num_ThirdParty1_max, f = function(x_row) ifelse(all(is.na(x_row)), NA, max(x_row, na.rm = T)))
# names(num_ThirdParty1_maxAll) <- "ThirdParty_1st_maxAll"
# num_ThirdParty1_sumAll <- analytics_byRow(num_ThirdParty1_sum, f = function(x_row) ifelse(all(is.na(x_row)), NA, sum(x_row, na.rm = T)))
# names(num_ThirdParty1_sumAll) <- "ThirdParty_1st_sumAll"

num_ThirdParty_list2_min <- lapply(num_ThirdParty_list2, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, min(x_row, na.rm = T))))
num_ThirdParty_list2_max <- lapply(num_ThirdParty_list2, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, max(x_row, na.rm = T))))
num_ThirdParty_list2_sum <- lapply(num_ThirdParty_list2, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, sum(x_row, na.rm = T))))
num_ThirdParty_list2_mean <- lapply(num_ThirdParty_list2, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, mean(x_row, na.rm = T))))
num_ThirdParty_list2_sd <- lapply(num_ThirdParty_list2, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, sd(x_row, na.rm = T))))
num_ThirdParty_list2_meanVsd <- lapply(num_ThirdParty_list2, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, (mean(x_row, na.rm = T) + 1) / (sd(x_row, na.rm = T) + 1))))

num_ThirdParty2_min <- do.call("cBind", num_ThirdParty_list2_min)
names(num_ThirdParty2_min) <- paste0("ThirdParty_Info_Period_any_", 13:17, "_min")
num_ThirdParty2_max <- do.call("cBind", num_ThirdParty_list2_max)
names(num_ThirdParty2_max) <- paste0("ThirdParty_Info_Period_any_", 13:17, "_max")
num_ThirdParty2_sum <- do.call("cBind", num_ThirdParty_list2_sum)
names(num_ThirdParty2_sum) <- paste0("ThirdParty_Info_Period_any_", 13:17, "_sum")
num_ThirdParty2_mean <- do.call("cBind", num_ThirdParty_list2_mean)
names(num_ThirdParty2_mean) <- paste0("ThirdParty_Info_Period_any_", 13:17, "_mean")
num_ThirdParty2_sd <- do.call("cBind", num_ThirdParty_list2_sd)
names(num_ThirdParty2_sd) <- paste0("ThirdParty_Info_Period_any_", 13:17, "_sd")
num_ThirdParty2_meanVsd <- do.call("cBind", num_ThirdParty_list2_meanVsd)
names(num_ThirdParty2_meanVsd) <- paste0("ThirdParty_Info_Period_any_", 13:17, "_meanVsd")
# num_ThirdParty2_minAll <- analytics_byRow(num_ThirdParty2_min, f = function(x_row) ifelse(all(is.na(x_row)), NA, min(x_row, na.rm = T)))
# names(num_ThirdParty2_minAll) <- "ThirdParty_2nd_minAll"
# num_ThirdParty2_maxAll <- analytics_byRow(num_ThirdParty2_max, f = function(x_row) ifelse(all(is.na(x_row)), NA, max(x_row, na.rm = T)))
# names(num_ThirdParty2_maxAll) <- "ThirdParty_2nd_maxAll"
# num_ThirdParty2_sumAll <- analytics_byRow(num_ThirdParty2_sum, f = function(x_row) ifelse(all(is.na(x_row)), NA, sum(x_row, na.rm = T)))
# names(num_ThirdParty2_sumAll) <- "ThirdParty_2nd_sumAll"

# class_names3 <- lapply(1:7, function(x) paste0("ThirdParty_Info_Period", rep(x, 12), "_", 1:12))
# class_names4 <- lapply(1:7, function(x) paste0("ThirdParty_Info_Period", rep(x, 5), "_", 13:17))
# num_ThirdParty_list3 <- lapply(class_names3, function(x) num_ThirdParty[, names(num_ThirdParty) %in% x])
# num_ThirdParty_list4 <- lapply(class_names4, function(x) num_ThirdParty[, names(num_ThirdParty) %in% x])
# num_ThirdParty_list3_min <- lapply(num_ThirdParty_list3, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, min(x_row, na.rm = T))))
# num_ThirdParty_list3_max <- lapply(num_ThirdParty_list3, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, max(x_row, na.rm = T))))
# num_ThirdParty_list3_sum <- lapply(num_ThirdParty_list3, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, sum(x_row, na.rm = T))))
# num_ThirdParty_list3_var <- lapply(num_ThirdParty_list3, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, var(x_row, na.rm = T))))
# num_ThirdParty3_min <- do.call("cBind", num_ThirdParty_list3_min)
# names(num_ThirdParty3_min) <- paste0("ThirdParty_Info_Period_", 1:7, "_1-12_min")
# num_ThirdParty3_max <- do.call("cBind", num_ThirdParty_list3_max)
# names(num_ThirdParty3_max) <- paste0("ThirdParty_Info_Period_", 1:7, "_1-12_max")
# num_ThirdParty3_sum <- do.call("cBind", num_ThirdParty_list3_sum)
# names(num_ThirdParty3_sum) <- paste0("ThirdParty_Info_Period_", 1:7, "_1-12_sum")
# num_ThirdParty3_var <- do.call("cBind", num_ThirdParty_list3_var)
# names(num_ThirdParty3_var) <- paste0("ThirdParty_Info_Period_", 1:7, "_1-12_var")
# 
# num_ThirdParty_list4_min <- lapply(num_ThirdParty_list4, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, min(x_row, na.rm = T))))
# num_ThirdParty_list4_max <- lapply(num_ThirdParty_list4, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, max(x_row, na.rm = T))))
# num_ThirdParty_list4_sum <- lapply(num_ThirdParty_list4, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, sum(x_row, na.rm = T))))
# num_ThirdParty_list4_var <- lapply(num_ThirdParty_list4, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, var(x_row, na.rm = T))))
# num_ThirdParty4_min <- do.call("cBind", num_ThirdParty_list4_min)
# names(num_ThirdParty4_min) <- paste0("ThirdParty_Info_Period_", 1:7, "_1-12_min")
# num_ThirdParty4_max <- do.call("cBind", num_ThirdParty_list4_max)
# names(num_ThirdParty4_max) <- paste0("ThirdParty_Info_Period_", 1:7, "_1-12_max")
# num_ThirdParty4_sum <- do.call("cBind", num_ThirdParty_list4_sum)
# names(num_ThirdParty4_sum) <- paste0("ThirdParty_Info_Period_", 1:7, "_1-12_sum")
# num_ThirdParty4_var <- do.call("cBind", num_ThirdParty_list4_var)
# names(num_ThirdParty4_var) <- paste0("ThirdParty_Info_Period_", 1:7, "_1-12_var")

num_ThirdParty3_1 <- cBind(num_ThirdParty1_min, num_ThirdParty1_max, num_ThirdParty1_sum, num_ThirdParty1_sd, num_ThirdParty1_meanVsd, num_ThirdParty2_min, num_ThirdParty2_max, num_ThirdParty2_sum, num_ThirdParty2_mean, num_ThirdParty2_sd, num_ThirdParty2_meanVsd)
# num_ThirdParty3_2 <- cBind(num_ThirdParty3_min, num_ThirdParty3_max, num_ThirdParty3_sum, num_ThirdParty3_var, num_ThirdParty4_min, num_ThirdParty4_max, num_ThirdParty4_sum, num_ThirdParty4_var)

num_ThirdParty5 <- num_ThirdParty1
num_ThirdParty5_1 <- lapply(num_ThirdParty5, function(x) as.numeric(cut(x, breaks = 100, labels = 1:100)))
num_ThirdParty5_1 <- data.frame(num_ThirdParty5_1, stringsAsFactors = F)
names(num_ThirdParty5_1) <- paste0(names(num_ThirdParty5_1), "_rank")
num_ThirdParty5_2 <- lapply(c(0, 0.2, 0.5, 0.8, 1), function(x) analytics_byRow(num_ThirdParty5_1, f = function(x_row) quantile(x_row, probs = x, na.rm = T)))
num_ThirdParty5_2 <- do.call("cBind", num_ThirdParty5_2)
names(num_ThirdParty5_2) <- paste0("ThirdParty_1st_rankSummary_", c(0, 0.2, 0.5, 0.8, 1))
num_ThirdParty5_3 <- lapply(c(-1, 0), function(x) analytics_byRow(num_ThirdParty_list1, f = function(x_row) sum(x_row == x, na.rm = NA)))
num_ThirdParty5_3 <- do.call("cBind", num_ThirdParty5_3)
names(num_ThirdParty5_3) <- c("ThirdParty_1st_is-1", "ThirdParty_1st_is0")
num_ThirdParty5 <- cBind(num_ThirdParty5_2)

num_ThirdParty6 <- num_ThirdParty2
num_ThirdParty6_1 <- lapply(num_ThirdParty6, function(x) as.numeric(cut(x, breaks = 100, labels = 1:100)))
num_ThirdParty6_1 <- data.frame(num_ThirdParty6_1, stringsAsFactors = F)
names(num_ThirdParty6_1) <- paste0(names(num_ThirdParty6_1), "_rank")
num_ThirdParty6_2 <- lapply(c(0, 0.2, 0.5, 0.8, 1), function(x) analytics_byRow(num_ThirdParty6_1, f = function(x_row) quantile(x_row, probs = x, na.rm = T)))
num_ThirdParty6_2 <- do.call("cBind", num_ThirdParty6_2)
names(num_ThirdParty6_2) <- paste0("ThirdParty_2nd_rankSummary_", c(0, 0.2, 0.5, 0.8, 1))
num_ThirdParty6_3 <- lapply(c(-1, 0), function(x) analytics_byRow(num_ThirdParty_list2, f = function(x_row) sum(x_row == x, na.rm = NA)))
num_ThirdParty6_3 <- do.call("cBind", num_ThirdParty6_3)
names(num_ThirdParty6_3) <- c("ThirdParty_2nd_is-1", "ThirdParty_2nd_is0")
num_ThirdParty6 <- cBind(num_ThirdParty6_2)

# num_ThirdParty5 <- num_ThirdParty1
# num_ThirdParty5_1 <- lapply(num_ThirdParty5, function(x) rank(x, na.last = "keep"))
# num_ThirdParty5_1 <- data.frame(num_ThirdParty5_1, stringsAsFactors = F)
# names(num_ThirdParty5_1) <- paste0(names(num_ThirdParty5_1), "_rank")
# num_ThirdParty5_2 <- lapply(c(0, 0.2, 0.5, 0.8, 1), function(x) analytics_byRow(num_ThirdParty5_1, f = function(x_row) quantile(x_row, probs = x, na.rm = T)))
# num_ThirdParty5_2 <- do.call("cBind", num_ThirdParty5_2)
# names(num_ThirdParty5_2) <- paste0("ThirdParty_1st_rankSummary_", c(0, 0.2, 0.5, 0.8, 1))
# num_ThirdParty5_3 <- lapply(c(-1, 0), function(x) analytics_byRow(num_ThirdParty_list1, f = function(x_row) sum(x_row == x, na.rm = NA)))
# num_ThirdParty5_3 <- do.call("cBind", num_ThirdParty5_3)
# names(num_ThirdParty5_3) <- c("ThirdParty_1st_is-1", "ThirdParty_1st_is0")
# num_ThirdParty5 <- cBind(num_ThirdParty5_2)
# 
# num_ThirdParty6 <- num_ThirdParty2
# num_ThirdParty6_1 <- lapply(num_ThirdParty6, function(x) rank(x, na.last = "keep"))
# num_ThirdParty6_1 <- data.frame(num_ThirdParty6_1, stringsAsFactors = F)
# names(num_ThirdParty6_1) <- paste0(names(num_ThirdParty6_1), "_rank")
# num_ThirdParty6_2 <- lapply(c(0, 0.2, 0.5, 0.8, 1), function(x) analytics_byRow(num_ThirdParty6_1, f = function(x_row) quantile(x_row, probs = x, na.rm = T)))
# num_ThirdParty6_2 <- do.call("cBind", num_ThirdParty6_2)
# names(num_ThirdParty6_2) <- paste0("ThirdParty_2nd_rankSummary_", c(0, 0.2, 0.5, 0.8, 1))
# num_ThirdParty6_3 <- lapply(c(-1, 0), function(x) analytics_byRow(num_ThirdParty_list2, f = function(x_row) sum(x_row == x, na.rm = NA)))
# num_ThirdParty6_3 <- do.call("cBind", num_ThirdParty6_3)
# names(num_ThirdParty6_3) <- c("ThirdParty_2nd_is-1", "ThirdParty_2nd_is0")
# num_ThirdParty6 <- cBind(num_ThirdParty6_2)

num_ThirdParty <- cBind(num_ThirdParty1, num_ThirdParty2, num_ThirdParty3_1, num_ThirdParty5, num_ThirdParty6)
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
dataMaster_ListingInfo1[60001:70000, 1] <- as.numeric(as.Date(dataMaster_ListingInfo1[60001:70000, 1], format = "%d/%m/%Y"))
dataMaster_ListingInfo2[1:60000, 1] <-  weekdays(as.Date(dataMaster_ListingInfo1[1:60000, 1], format = "%Y-%m-%d"))
dataMaster_ListingInfo2[60001:70000, 1] <- weekdays(as.Date(dataMaster_ListingInfo1[60001:70000, 1], format = "%d/%m/%Y"))
dataMaster_ListingInfo_onehot <- lapply(dataMaster_ListingInfo2, one_hot)
dataMaster_ListingInfo_onehot <- do.call("cBind", dataMaster_ListingInfo_onehot)
names(dataMaster_ListingInfo_onehot) <- paste0("ListingInfo_onehot_", names(dataMaster_ListingInfo_onehot))
dataMaster_ListingInfo <- cBind(dataMaster_ListingInfo1, dataMaster_ListingInfo_onehot)
# =======================================================================
# 组合全部数据 ----------------------------------------------------------
dataMaster_specialNewXs <- cBind(Idx = Idx, dataMaster_UserInfo, cat_Education, num_ThirdParty, dataMaster_WeblogInfo, dataMaster_SocialNetwork, dataMaster_ListingInfo)
dataMaster_Train <- subset(dataMaster_specialNewXs, Idx %in% train_Idx)
dataMaster_Test <- subset(dataMaster_specialNewXs, Idx %in% test_Idx)
# =======================================================================
# =======================================================================