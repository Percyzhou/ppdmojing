# 处理UserInfo中的类别变量 -------------------------------------------------
# 读取各省份经济信息 -------------------------------------------------------
pvn <- read.csv("config/province mac datas.csv", fileEncoding = "gbk", stringsAsFactors = F)
pvn$省 <- vapply(pvn$省, function(x) str_sub(str_trim(x), start = 1, end = 2), character(1))
# ==========================================================================
# 读取各城市经济发展信息 ---------------------------------------------------
city <- read.xlsx("config/城市发展程度表.xlsx", sheetIndex = 1, header = T, as.data.frame = T, stringsAsFactors = F)
# ==========================================================================
cat_dataMaster <- readRDS("dataNew/cat_dataMaster")#读取类别数据
# ==========================================================================
# 提取出所有的UserInfo特征 -------------------------------------------------
cat_UserInfo <- cat_dataMaster[, grepl("^UserInfo_", names(cat_dataMaster))]
# ==========================================================================
# 替换空值 -----------------------------------------------------------------
cat_UserInfo[is.na(cat_UserInfo)] <- NA
cat_UserInfo[cat_UserInfo == ""] <- NA
cat_UserInfo[cat_UserInfo == "不详"] <- NA
cat_UserInfo[cat_UserInfo == "D"] <- NA
# ==========================================================================
# 用户地理信息 -------------------------------------------------------------
# 处理省份变量 -------------------------------------------------------------
provienceInfo <- cat_UserInfo[, c("UserInfo_7", "UserInfo_19")]
provienceInfo[] <- lapply(provienceInfo, function(x) str_sub(str_trim(x), start = 1, end = 2))
provienceInfo_onehot <- lapply(provienceInfo, one_hot)
provienceInfo_onehot <- do.call("cBind", provienceInfo_onehot)
names(provienceInfo_onehot) <- paste0("provienceInfo_", names(provienceInfo_onehot))
provienceInfo_count <- count_theCategory(data = provienceInfo)
provienceInfo_count <- lapply(provienceInfo_count, function(x) rank(x, na.last = "keep"))
provienceInfo_count <- data.frame(provienceInfo_count, stringsAsFactors = F)
provienceInfo_stay1 <- provienceInfo[, "UserInfo_7", drop = F]
provienceInfo_stay2 <- provienceInfo[, "UserInfo_19", drop = F]
provienceInfo_economic1 <- lapply(provienceInfo_stay1[[1]], function(x) unlist(ifelse(is.na(x), list(matrix(NA, nrow = 1, ncol = 11)), list(subset(pvn, 省 == x)[, 2:12]))))
provienceInfo_economic1 <- do.call("rBind", provienceInfo_economic1)
provienceInfo_economic2 <- lapply(provienceInfo_stay2[[1]], function(x) unlist(ifelse(is.na(x), list(matrix(NA, nrow = 1, ncol = 11)), list(subset(pvn, 省 == x)[, 2:12]))))
provienceInfo_economic2 <- do.call("rBind", provienceInfo_economic2)
provienceInfo_economic3 <- (provienceInfo_economic1 - provienceInfo_economic2)[, "gdp", drop = F]
provienceInfo <- cBind(provienceInfo_onehot, provienceInfo_count, provienceInfo_economic1, provienceInfo_economic2, provienceInfo_economic3)
# 处理城市变量 -------------------------------------------------------------
cityInfo <- cat_UserInfo[, c("UserInfo_2", "UserInfo_4", "UserInfo_8", "UserInfo_20")]
cityInfo_raw <- cityInfo
cityInfo[] <- lapply(cityInfo, function(x) str_sub(str_trim(x), start = 1, end = 2))
cityInfo[] <- lapply(cityInfo, function(x) {
  out <- rep(NA,length(x))
  for(k in 1:length(x)){
    out[k] <- ifelse(is.na(x[k]), NA, subset(city, grepl(x[k], 城市名称))[["城市类型"]][1])
  }
  out[is.na(out) & !is.na(x)] <- "四线城市"
  return(out)
})
cityInfo_class <- cityInfo
cityInfo <- lapply(cityInfo, one_hot)
cityInfo <- do.call("cBind", cityInfo)
names(cityInfo) <- paste0("cityInfo_", names(cityInfo))
cityInfo_class[] <- lapply(cityInfo_class, function(x) str_sub(str_trim(x), start = 1, end = 4))
cityInfo_class <- lapply(cityInfo_class, one_hot)
cityInfo_class <- do.call("cBind", cityInfo)
names(cityInfo_class) <- paste0("cityInfo_", names(cityInfo_class))
cityInfo_num <- analytics_byRow(data = cityInfo_raw, f = function(x) length(table(x)))
cityInfo_max <- analytics_byRow(data = cityInfo_raw, f = function(x) ifelse(all(is.na(x)), 0, max(table(x), na.rm = T)))
cityInfo <- cBind(cityInfo, cityInfo_class, cityInfo_num, cityInfo_max)

villageInfo <- cat_UserInfo[, "UserInfo_24", drop = F]
villageInfo[!is.na(villageInfo)] <- 1

# dataMaster_UserInfo_place_provience_woe1 <- map_woe(x = unlist(dataMaster_UserInfo_place_provience_stay1[1:length(target),]), y = unlist(dataMaster_UserInfo_place_provience_stay1), target = target, Threshold = 100, name = province_woe1)
# dataMaster_UserInfo_place_provience_woe2 <- map_woe(x = unlist(dataMaster_UserInfo_place_provience_stay2[1:length(target),]), y = unlist(dataMaster_UserInfo_place_provience_stay2), target = target, Threshold = 100, name = province_woe1)
# 组合全部用户地理信息 -----------------------------------------------------
positionInfo <- cBind(provienceInfo, cityInfo, villageInfo)
# ==========================================================================
# 对移动供应商、婚姻情况、学历信息进行one hot处理和count处理 ---------------
cat_UserInfo_other<- cat_UserInfo[, vapply(c(1, 3, 5, 6, 9, 11, 12, 13, 14, 15, 16, 17, 21, 22, 23), function(x) paste0("UserInfo_", x), character(1))]
cat_UserInfo_other1 <- lapply(cat_UserInfo_other, FUN = one_hot)
cat_UserInfo_other1 <- do.call("cBind", cat_UserInfo_other1)
cat_UserInfo_other2 <- count_theCategory(cat_UserInfo[, "UserInfo_23", drop = F])
cat_UserInfo_other3 <- mark_sparseValue(cat_UserInfo_other)
cat_UserInfo_other <- cBind(cat_UserInfo_other1, cat_UserInfo_other2, cat_UserInfo_other3)
cat_UserInfo <- cBind(positionInfo, cat_UserInfo_other)
# ==========================================================================

# 处理UserInfo的数值变量 ---------------------------------------------------
num_dataMaster <- readRDS("dataNew/num_dataMaster")
num_UserInfo <- num_dataMaster[, c("UserInfo_10", "UserInfo_18")]
# ==========================================================================
# 将所有UserInfo的变量组合起来 ---------------------------------------------
dataMaster_UserInfo <- cBind(cat_UserInfo, num_UserInfo)
# ==========================================================================