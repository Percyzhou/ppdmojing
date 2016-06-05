# 读取dataUserupdate数据 -----------------------------------------------------------------
dataUserupdate1_1 <- read.csv("data/Train/trainUserupdate.csv", fileEncoding = "gbk", stringsAsFactors = F)
dataUserupdate1_2 <- read.csv("data/Test_A/a_testUserupdate1.csv", fileEncoding = "gbk", stringsAsFactors = F)
dataUserupdate1_3 <- read.csv("data/Test_A/a_testUserupdate2.csv", fileEncoding = "gbk", stringsAsFactors = F)
dataUserupdate1 <- rBind(dataUserupdate1_1, dataUserupdate1_2, dataUserupdate1_3)
dataUserupdate2 <- read.csv("data/Test_B/b_testUserupdate.csv", fileEncoding = "gbk", stringsAsFactors = F)

train_Idx <- dataUserupdate1$Idx
test_Idx <- dataUserupdate2$Idx
dataUserupdate <- rBind(dataUserupdate1, dataUserupdate2)
# =================================================================================
# 提取"Idx"、"ListingInfo1"、"UserupdateInfo1"、"UserupdateInfo2"子变量空间 ---------
dataUserupdate_Idx <- dataUserupdate[, "Idx", drop = T]
dataUserupdate_UserupdateInfo1 <- dataUserupdate[, "UserupdateInfo1", drop = F]
dataUserupdate_UserupdateInfo1[] <- lapply(dataUserupdate_UserupdateInfo1, tolower)
# dataUserupdate_UserupdateInfo1[["UserupdateInfo1"]] <- replaceChar2(dataUserupdate_UserupdateInfo1[["UserupdateInfo1"]], char = list(c("_realname", "_age", "_gender", "_nickname"),
#                                                                                                                                      c("_companysizeid", "_companytypeid", "_workyears", "_department", "_companyaddress", "_companyname"),
#                                                                                                                                      c("_hasbussinesslicense", "_bussinessaddress", "_webshopurl", "_webshoptypeid", "_otherwebshoptype"),
#                                                                                                                                      c("_companyphone", "_secondemail", "_secondmobile", "_phone", "_phonetype", "_dormitoryphone", "_relationshipid"),
#                                                                                                                                      c("_graduatedate", "_graduateschool", "_schoolname", "_educationid"),
#                                                                                                                                      c("_hasppdaiaccount", "_ppdaiaccount", "_idaddress", "_orderid", "_contactid", "_userid", "_creationdate",  "_hassborgjj", "_position", "_incomefrom")),
#                                                                    replaceChar = list("personal", "work", "bussiness", "conaction", "education", "rare"))
dataUserupdate_UserupdateInfo2 <- dataUserupdate[, "UserupdateInfo2", drop = F]
# =================================================================================
# 分别对各子集进行处理 ------------------------------------------------------------
dataUserupdate_UserupdateInfo1 <- one_hot(unlist(dataUserupdate_UserupdateInfo1))
dataUserupdate_UserupdateInfo1[dataUserupdate_UserupdateInfo1 == 0] <- NA
names(dataUserupdate_UserupdateInfo1) <- paste0("UserupdateInfo1_", names(dataUserupdate_UserupdateInfo1))
dataUserupdate_UserupdateInfo2 <- data.frame(dataUserupdate_UserupdateInfo2 = as.numeric(as.Date(dataUserupdate_UserupdateInfo2$UserupdateInfo2, "%Y/%m/%d")))
# =================================================================================
# 分别生成各种信息 ----------------------------------------------------------------
# 客户修改次数 --------------------------------------------------------------------
dataUserupdate_count_by_Times <- c(table(dataUserupdate_Idx))
# 客户修改了多少批数据 ------------------------------------------------------------
dataUserupdate_count_by_Days_table <- table(data.frame(dataUserupdate_Idx, dataUserupdate_UserupdateInfo2))
dataUserupdate_count_by_Days_table[dataUserupdate_count_by_Days_table != 0] <- 1
dataUserupdate_count_by_Days <- apply(dataUserupdate_count_by_Days_table, MARGIN = 1, sum)
# 客户在各类别中修改了多少次 ------------------------------------------------------
names_dataUserupdate_UserupdateInfo1 <- names(dataUserupdate_UserupdateInfo1)
dataUserupdate_UserupdateInfo1 <- lapply(dataUserupdate_UserupdateInfo1, function(x){
  data <- data.frame(dataUserupdate_Idx, x)
  data.frame(table(data))[[3]]
})
dataUserupdate_UserupdateInfo1 <- do.call("cBind", dataUserupdate_UserupdateInfo1)
dataUserupdate_UserupdateInfo1 <- data.frame(dataUserupdate_UserupdateInfo1, stringsAsFactors = F)
names(dataUserupdate_UserupdateInfo1) <- names_dataUserupdate_UserupdateInfo1
dataUserupdate_UserupdateInfo1[dataUserupdate_UserupdateInfo1 >  0] <- 1
rate_col <- vapply(dataUserupdate_UserupdateInfo1, function(x) sum(x == 0) / length(x), numeric(1))
dataUserupdate_UserupdateInfo1 <- dataUserupdate_UserupdateInfo1[, rate_col < 0.975]
# dataUserupdate_UserupdateInfo1_min <- analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, min(x_row, na.rm = T)))
# dataUserupdate_UserupdateInfo1_max <- analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, max(x_row, na.rm = T)))
# dataUserupdate_UserupdateInfo1_sum <- analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, sum(x_row, na.rm = T)))

# 客户修改的时间差距 --------------------------------------------------------------
# =================================================================================
# 合并相同Idx的样本 ---------------------------------------------------------------
dataUserupdate_Idx <- unique(dataUserupdate_Idx[order(dataUserupdate_Idx)])
dataUserupdate <- cBind(Idx = dataUserupdate_Idx, Userupdate_count_by_Times = dataUserupdate_count_by_Times,
                        Userupdate_count_by_Days = dataUserupdate_count_by_Days, dataUserupdate_UserupdateInfo1, stringsAsFactors = F)
saveRDS(dataUserupdate, "dataNew/dataUserupdate")
dataUserupdate <- readRDS("dataNew/dataUserupdate")
# =================================================================================
# 分别对各子集进行处理 ------------------------------------------------------------
dataUserupdate_Train <- subset(dataUserupdate, Idx %in% train_Idx)
dataUserupdate_Test <- subset(dataUserupdate, Idx %in% test_Idx)
# =================================================================================
