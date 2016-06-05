# 读取dataMaster数据 ------------------------------
target <- readRDS("dataNew/targetVar")
num_dataMaster <- readRDS("dataNew/num_dataMaster")
cat_dataMaster <- readRDS("dataNew/cat_dataMaster")
# =================================================
# 数值变量出 --------------------------------------
rateSparse <- vapply(num_dataMaster, function(x) sum(is.na(x)) / length(x), numeric(1))
# rateLarge <- vapply(num_dataMaster, function(x) max(table(x)) / length(x), numeric(1))
selectbySparse <- rateSparse > 0.5
# selectbyLarge <- rateLarge > 0.95
for(x in names(num_dataMaster)[selectbySparse]){
  num_dataMaster[[x]] <- NULL
}
# =================================================
cat_dataMaster[] <- lapply(cat_dataMaster, tolower)
# 清理UserInfo数据 --------------------------------
numPlus <- cat_dataMaster[, c("UserInfo_1", "UserInfo_3", "UserInfo_14", "UserInfo_15", "UserInfo_16")]
for(x in c("UserInfo_1", "UserInfo_3", "UserInfo_14", "UserInfo_15", "UserInfo_16")){
  cat_dataMaster[, x] <- NULL
}
# 整理省份数据 ------------------------------------
pvn <- read.csv("config/province mac datas.csv", fileEncoding = "gbk", stringsAsFactors = F)
pvn$省 <- vapply(pvn$省, function(x) str_sub(str_trim(x), start = 1, end = 2), character(1))

provienceInfo <- cat_dataMaster[, c("UserInfo_7", "UserInfo_19")]
provienceInfo[] <- lapply(provienceInfo, function(x) str_sub(str_trim(x), start = 1, end = 2))
provienceInfo_stay1 <- provienceInfo[, "UserInfo_7", drop = F]
provienceInfo_stay2 <- provienceInfo[, "UserInfo_19", drop = F]
provienceInfo_economic1 <- lapply(provienceInfo_stay1[[1]], function(x) unlist(ifelse(x == "不详", list(matrix(NA, nrow = 1, ncol = 8)), list(subset(pvn, 省 == x)[, c(2:8, 12)]))))
provienceInfo_economic1 <- do.call("rBind", provienceInfo_economic1)
provienceInfo_economic2 <- lapply(provienceInfo_stay2[[1]], function(x) unlist(ifelse(x == "不详", list(matrix(NA, nrow = 1, ncol = 8)), list(subset(pvn, 省 == x)[, c(2:8, 12)]))))
provienceInfo_economic2 <- do.call("rBind", provienceInfo_economic2)
provienceInfo_economic3 <- (provienceInfo_economic1 - provienceInfo_economic2)[, "gdp", drop = F]
num_provienceInfo <- cBind(provienceInfo_economic1, provienceInfo_economic2, provienceInfo_economic3)
names(num_provienceInfo) <- paste0("provienceEconomicInfo_", names(num_provienceInfo))
# 整理城市数据 ------------------------------------
cityInfo_add1 <- cat_dataMaster[["UserInfo_4"]]
cityInfo_add2_1 <- str_replace(cat_dataMaster[["UserInfo_24"]], pattern = cat_dataMaster[["UserInfo_19"]], replacement = "")
cityInfo_add2_1[cityInfo_add2_1 == "d"] <- "不详"
cityInfo_add2_2 <- cat_dataMaster[["UserInfo_2"]]
cat_dataMaster[["UserInfo_8"]] <- str_replace(cat_dataMaster[["UserInfo_8"]], pattern = "不详", replacement = cityInfo_add1)
cat_dataMaster[["UserInfo_20"]] <- str_replace(cat_dataMaster[["UserInfo_20"]], pattern = "不详", replacement = cityInfo_add2_1)
cat_dataMaster[["UserInfo_20"]] <- str_replace(cat_dataMaster[["UserInfo_20"]], pattern = "不详", replacement = cityInfo_add2_2)
# 处理城市特征-------------------------------------
city <- read.xlsx("config/城市发展程度表old.xlsx", sheetIndex = 1, header = T, as.data.frame = T, stringsAsFactors = F)
cityInfo <- cat_dataMaster[, c("UserInfo_2", "UserInfo_4", "UserInfo_8", "UserInfo_20")]
cityInfo[] <- lapply(cityInfo, function(x) str_sub(str_trim(x), start = 1, end = 2))
cityInfo[] <- lapply(cityInfo, function(x) {
  out <- rep(NA,length(x))
  for(k in 1:length(x)){
    out[k] <- ifelse(is.na(x[k]), NA, subset(city, grepl(x[k], 城市名称))[["城市类型"]][1])
    out[k] <- ifelse(x[k] == "" | x[k] == "不详", "不详", out[k])
  }
  out[is.na(out) & !is.na(x)] <- "四线城市"
  return(out)
})
cat_dataMaster[, c("UserInfo_2", "UserInfo_4", "UserInfo_8", "UserInfo_20")] <- cityInfo
cityInfo_lowHigh <- cityInfo
cityInfo_lowHigh <- lapply(cityInfo_lowHigh, function(x) as.numeric(replaceChar(x, char = c("一线城市（强）", "一线城市（中）", "一线城市（弱）", "一线城市（准）",
                                                                                            "二线城市（强）", "二线城市（中）", "二线城市（弱）", "二线城市（准）",
                                                                                            "三线城市（强）", "三线城市（中）", "三线城市（弱）", "三线城市（准）",
                                                                                            "四线城市", "不详"), replaceChar = c(0:12, NA))))

city_lowtohigh <- with(cityInfo_lowHigh, UserInfo_8 - UserInfo_20)
flag_lowtohigh <- city_lowtohigh < 0
city_lowtohigh[flag_lowtohigh] <- 1
city_lowtohigh[!flag_lowtohigh] <- 0

city_hightolow <- with(cityInfo_lowHigh, UserInfo_8 - UserInfo_20)
flag_hightolow <- city_hightolow > 0
city_hightolow[flag_hightolow] <- 1
city_hightolow[!flag_hightolow] <- 0

# ==========================================================
# cat_dataMaster[["UserInfo_5"]] <- replaceChar(cat_dataMaster[["UserInfo_5"]], char = 1, replaceChar = NA)
# cat_dataMaster[["UserInfo_6"]] <- replaceChar(cat_dataMaster[["UserInfo_6"]], char = 1, replaceChar = NA)
# cat_dataMaster[["UserInfo_9"]] <- replaceChar(cat_dataMaster[["UserInfo_9"]], char = c("中国移动", "中国联通"), replaceChar = c(NA, NA))
# cat_dataMaster[["UserInfo_11"]] <- replaceChar(cat_dataMaster[["UserInfo_11"]], char = c(NA, 1), replaceChar = c("NA", NA))
# cat_dataMaster[["UserInfo_12"]] <- replaceChar(cat_dataMaster[["UserInfo_12"]], char = NA, replaceChar = "NA")
# cat_dataMaster[["UserInfo_13"]] <- replaceChar(cat_dataMaster[["UserInfo_13"]], char = NA, replaceChar = "NA")
# cat_dataMaster[["UserInfo_21"]] <- replaceChar(cat_dataMaster[["UserInfo_21"]], char = NA, replaceChar = "NA")
# cat_dataMaster[["UserInfo_22"]] <- replaceChar(cat_dataMaster[["UserInfo_22"]], char = "d", replaceChar = NA)
cat_dataMaster[["UserInfo_22"]] <- arrangeChar(cat_dataMaster[["UserInfo_22"]], charLists = list(c("已婚", "复婚", "初婚", "再婚", "丧偶", "不详")))
villageInfo <- cat_dataMaster[["UserInfo_24"]]
villageInfo[villageInfo == "d" | villageInfo == 28] <- 0
villageInfo[villageInfo != 0] <- 1
cat_dataMaster[["UserInfo_24"]] <- villageInfo
# cat_dataMaster[["UserInfo_23"]] <- replaceChar(cat_dataMaster[["UserInfo_23"]], char = "d", replaceChar = NA)
# =================================================
# 清理Education_Info数据 --------------------------
# cat_dataMaster[["Education_Info2"]] <- replaceChar(cat_dataMaster[["Education_Info2"]], char = "b", replaceChar = NA)
cat_dataMaster[["Education_Info2"]] <- arrangeChar(cat_dataMaster[["Education_Info2"]], charLists = list(c("an", "aq", "u")))
cat_dataMaster[["Education_Info3"]] <- arrangeChar(cat_dataMaster[["Education_Info3"]], charLists = list(c("毕业", "结业")))
cat_dataMaster[["Education_Info4"]] <- arrangeChar(cat_dataMaster[["Education_Info4"]], charLists = list(c("ae", "ar", "v")))
# cat_dataMaster[["Education_Info5"]] <- replaceChar(cat_dataMaster[["Education_Info2"]], char = 0, replaceChar = NA)
# cat_dataMaster[["Education_Info6"]] <- replaceChar(cat_dataMaster[["Education_Info6"]], char = "e", replaceChar = NA)
cat_dataMaster[["Education_Info6"]] <- arrangeChar(cat_dataMaster[["Education_Info6"]], charLists = list(c("am", "aq", "b", "u")))
# cat_dataMaster[["Education_Info7"]] <- replaceChar(cat_dataMaster[["Education_Info7"]], char = "e", replaceChar = NA)
# cat_dataMaster[["Education_Info8"]] <- replaceChar(cat_dataMaster[["Education_Info8"]], char = c(80, "e", "f"), replaceChar = c(NA, NA, NA))
cat_dataMaster[["Education_Info8"]] <- arrangeChar(cat_dataMaster[["Education_Info8"]], charLists = list(c("不详", "v", "ae")))
# =================================================
# 清理Weblog_Info数据 -----------------------------
# cat_dataMaster[["WeblogInfo_19"]] <- replaceChar(cat_dataMaster[["WeblogInfo_19"]], char = c("", "f", "h", "i", "j"), replaceChar = c("NA", NA, NA, NA, NA))
# cat_dataMaster[["WeblogInfo_20"]] <- replaceChar(cat_dataMaster[["WeblogInfo_20"]], char = c("", "c13", "c14", "c32", "c38", "c39", "i1", "i2", "i6", "i7", "i8", "o", "u"), replaceChar = c("NA", rep(NA, 12)))
cat_dataMaster[["WeblogInfo_20"]] <- arrangeChar(cat_dataMaster[["WeblogInfo_20"]], charLists = list(c("c1", "c12", "c17"), c("c15", "c16"), paste0("f", 1:16)))
# cat_dataMaster[["WeblogInfo_21"]] <- replaceChar(cat_dataMaster[["WeblogInfo_21"]], char = c("", "c", "d"), replaceChar = c("NA", NA, NA))
cat_Idx <- cat_dataMaster$Idx
cat_dataMaster$Idx <- NULL
cat_dataMaster[is.na(cat_dataMaster)] <- "N1"
cat_dataMaster[] <- lapply(cat_dataMaster, function(x){
  x_predict <- cBind(Idx = cat_Idx, x = x, stringsAsFactors = F)
  y_target <- target
  data <- merge(x_predict, y_target, by = "Idx", all = F)
  charForNA <- auto_char(x = data$x, target = data$target)
  replaceChar(x, char = charForNA, replaceChar = rep(NA, length(charForNA)))
})
cat_dataMaster <- cBind(Idx = cat_Idx, cat_dataMaster)
cat_dataMaster$UserInfo_25 <- city_lowtohigh
cat_dataMaster$UserInfo_26 <- city_hightolow
# =================================================
# num_ThirdParty <- num_dataMaster[, grepl("ThirdParty_Info_Period", names(num_dataMaster))]
# num_ThirdParty6 <- num_dataMaster[, grepl("ThirdParty_Info_Period6_", names(num_dataMaster))]
# num_ThirdParty7 <- num_dataMaster[, grepl("ThirdParty_Info_Period7_", names(num_dataMaster))]
# num_ThirdParty[num_ThirdParty == -1] <- NA
# num_ThirdParty6[num_ThirdParty6 == -1] <- -1
# num_ThirdParty7[num_ThirdParty7 == -1] <- -10
# num_dataMaster[, grepl("ThirdParty_Info_Period", names(num_dataMaster))] <- num_ThirdParty
# num_dataMaster[, grepl("ThirdParty_Info_Period6_", names(num_dataMaster))] <- num_ThirdParty6
# num_dataMaster[, grepl("ThirdParty_Info_Period7_", names(num_dataMaster))] <- num_ThirdParty7
# =================================================
num_dataMaster <- cBind(num_dataMaster, numPlus, num_provienceInfo)
saveRDS(num_dataMaster,"dataNew/num_dataMaster")
saveRDS(cat_dataMaster, "dataNew/cat_dataMaster")