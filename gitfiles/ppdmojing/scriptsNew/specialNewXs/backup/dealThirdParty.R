# 处理ThirdParty数据 ----------------------------------------------------------
num_dataMaster <- readRDS("dataNew/num_dataMaster")
num_ThirdParty <- num_dataMaster[, grepl("^ThirdParty_Info_Period", names(num_dataMaster))]
# =============================================================================
# 设置ThirdParty数据中"-1"位空值 ----------------------------------------------
# num_ThirdParty[num_ThirdParty == 0] <- NA
# num_ThirdParty1 <- num_ThirdParty2 <- num_ThirdParty
# =============================================================================
class_names1 <- vapply(1:12, function(x) paste0("_", x, "$"), character(1))
class_names2 <- vapply(13:17, function(x) paste0("_", x, "$"), character(1))
num_ThirdParty_list1 <- lapply(class_names1, function(x) num_ThirdParty[, grepl(x, names(num_ThirdParty))])
num_ThirdParty_list2 <- lapply(class_names2, function(x) num_ThirdParty[, grepl(x, names(num_ThirdParty))])
num_ThirdParty1 <- do.call("cBind", num_ThirdParty_list1)
num_ThirdParty1 <- lapply(num_ThirdParty1, function(x) as.numeric(cut(x, breaks = 600)))
num_ThirdParty1 <- do.call("cBind", num_ThirdParty1)
num_ThirdParty1 <- data.frame(num_ThirdParty1, stringsAsFactors = F)
num_ThirdParty2 <- do.call("cBind", num_ThirdParty_list2)
num_ThirdParty2 <- lapply(num_ThirdParty2, function(x) as.numeric(cut(x, breaks = 500)))
num_ThirdParty2 <- do.call("cBind", num_ThirdParty2)
num_ThirdParty2 <- data.frame(num_ThirdParty2, stringsAsFactors = F)
# ================================================================================================
# 对ThirdParty数据进行划分 ----------------------------------------------------
num_ThirdParty_list1_min <- lapply(num_ThirdParty_list1, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, min(x_row, na.rm = T))))
num_ThirdParty_list1_max <- lapply(num_ThirdParty_list1, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, max(x_row, na.rm = T))))
num_ThirdParty_list1_sum <- lapply(num_ThirdParty_list1, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, sum(x_row, na.rm = T))))
num_ThirdParty1_min <- do.call("cBind", num_ThirdParty_list1_min)
names(num_ThirdParty1_min) <- paste0("ThirdParty_Info_Period_any_", 1:12, "_min")
num_ThirdParty1_max <- do.call("cBind", num_ThirdParty_list1_max)
names(num_ThirdParty1_max) <- paste0("ThirdParty_Info_Period_any_", 1:12, "_max")
num_ThirdParty1_sum <- do.call("cBind", num_ThirdParty_list1_sum)
names(num_ThirdParty1_sum) <- paste0("ThirdParty_Info_Period_any_", 1:12, "_sum")
num_ThirdParty1_minAll <- analytics_byRow(num_ThirdParty1_min, f = function(x_row) ifelse(all(is.na(x_row)), NA, min(x_row, na.rm = T)))
names(num_ThirdParty1_minAll) <- "ThirdParty_1st_minAll"
num_ThirdParty1_maxAll <- analytics_byRow(num_ThirdParty1_max, f = function(x_row) ifelse(all(is.na(x_row)), NA, max(x_row, na.rm = T)))
names(num_ThirdParty1_maxAll) <- "ThirdParty_1st_maxAll"
num_ThirdParty1_sumAll <- analytics_byRow(num_ThirdParty1_sum, f = function(x_row) ifelse(all(is.na(x_row)), NA, sum(x_row, na.rm = T)))
names(num_ThirdParty1_sumAll) <- "ThirdParty_1st_sumAll"

num_ThirdParty_list2_min <- lapply(num_ThirdParty_list2, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, min(x_row, na.rm = T))))
num_ThirdParty_list2_max <- lapply(num_ThirdParty_list2, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, max(x_row, na.rm = T))))
num_ThirdParty_list2_sum <- lapply(num_ThirdParty_list2, function(x) analytics_byRow(x, f = function(x_row) ifelse(all(is.na(x_row)), NA, sum(x_row, na.rm = T))))
num_ThirdParty2_min <- do.call("cBind", num_ThirdParty_list2_min)
names(num_ThirdParty2_min) <- paste0("ThirdParty_Info_Period_any_", 13:17, "_min")
num_ThirdParty2_max <- do.call("cBind", num_ThirdParty_list2_max)
names(num_ThirdParty2_max) <- paste0("ThirdParty_Info_Period_any_", 13:17, "_max")
num_ThirdParty2_sum <- do.call("cBind", num_ThirdParty_list2_sum)
names(num_ThirdParty2_sum) <- paste0("ThirdParty_Info_Period_any_", 13:17, "_sum")
num_ThirdParty2_minAll <- analytics_byRow(num_ThirdParty2_min, f = function(x_row) ifelse(all(is.na(x_row)), NA, min(x_row, na.rm = T)))
names(num_ThirdParty2_minAll) <- "ThirdParty_2nd_minAll"
num_ThirdParty2_maxAll <- analytics_byRow(num_ThirdParty2_max, f = function(x_row) ifelse(all(is.na(x_row)), NA, max(x_row, na.rm = T)))
names(num_ThirdParty2_maxAll) <- "ThirdParty_2nd_maxAll"
num_ThirdParty2_sumAll <- analytics_byRow(num_ThirdParty2_sum, f = function(x_row) ifelse(all(is.na(x_row)), NA, sum(x_row, na.rm = T)))
names(num_ThirdParty2_sumAll) <- "ThirdParty_2nd_sumAll"
num_ThirdParty3 <- cBind(num_ThirdParty1_min, num_ThirdParty1_max, num_ThirdParty1_sum, num_ThirdParty2_min, num_ThirdParty2_max, num_ThirdParty2_sum)

num_ThirdParty4 <- num_ThirdParty1
num_ThirdParty4_1 <- lapply(num_ThirdParty4, function(x) rank(x, na.last = "keep"))
num_ThirdParty4_1 <- data.frame(num_ThirdParty4_1, stringsAsFactors = F)
names(num_ThirdParty4_1) <- paste0(names(num_ThirdParty4_1), "_rank")
num_ThirdParty4_2 <- lapply(c(0, 0.2, 0.5, 0.8, 1), function(x) analytics_byRow(num_ThirdParty4_1, f = function(x_row) quantile(x_row, probs = x, na.rm = T)))
num_ThirdParty4_2 <- do.call("cBind", num_ThirdParty4_2)
names(num_ThirdParty4_2) <- paste0("ThirdParty_1st_rankSummary_", c(0, 0.2, 0.5, 0.8, 1))
num_ThirdParty4_3 <- lapply(c(-1, 0), function(x) analytics_byRow(num_ThirdParty_list1, f = function(x_row) sum(x_row == x, na.rm = NA)))
num_ThirdParty4_3 <- do.call("cBind", num_ThirdParty4_3)
names(num_ThirdParty4_3) <- c("ThirdParty_1st_is-1", "ThirdParty_1st_is0")
num_ThirdParty4 <- cBind(num_ThirdParty4_1, num_ThirdParty4_2, num_ThirdParty4_3)

num_ThirdParty5 <- num_ThirdParty2
num_ThirdParty5_1 <- lapply(num_ThirdParty5, function(x) rank(x, na.last = "keep"))
num_ThirdParty5_1 <- data.frame(num_ThirdParty5_1, stringsAsFactors = F)
names(num_ThirdParty5_1) <- paste0(names(num_ThirdParty5_1), "_rank")
num_ThirdParty5_2 <- lapply(c(0, 0.2, 0.5, 0.8, 1), function(x) analytics_byRow(num_ThirdParty5_1, f = function(x_row) quantile(x_row, probs = x, na.rm = T)))
num_ThirdParty5_2 <- do.call("cBind", num_ThirdParty5_2)
names(num_ThirdParty5_2) <- paste0("ThirdParty_2nd_rankSummary_", c(0, 0.2, 0.5, 0.8, 1))
num_ThirdParty5_3 <- lapply(c(-1, 0), function(x) analytics_byRow(num_ThirdParty_list2, f = function(x_row) sum(x_row == x, na.rm = NA)))
num_ThirdParty5_3 <- do.call("cBind", num_ThirdParty5_3)
names(num_ThirdParty5_3) <- c("ThirdParty_2nd_is-1", "ThirdParty_2nd_is0")
num_ThirdParty5 <- cBind(num_ThirdParty5_1, num_ThirdParty5_2, num_ThirdParty5_3)
# num_ThirdParty3 <- lapply(num_ThirdParty3, function(x) as.numeric(cut(x, breaks = 500)))
# num_ThirdParty3 <- do.call("cBind", num_ThirdParty3)
# num_ThirdParty3 <- data.frame(num_ThirdParty3, stringsAsFactors = F)
# num_ThirdParty2_sparseRate1 <- lapply(num_ThirdParty2_list1, function(x) analytics_byRow(x, f = function(x_row) sum(is.na(x_row)) / length(x_row)))
# num_ThirdParty2_sparseRate2 <- lapply(num_ThirdParty2_list2, function(x) analytics_byRow(x, f = function(x_row) sum(is.na(x_row)) / length(x_row)))
# num_ThirdParty2_sparseRate1 <- do.call("cBind", num_ThirdParty2_sparseRate1)
# num_ThirdParty2_sparseRate2 <- do.call("cBind", num_ThirdParty2_sparseRate2)
# num_ThirdParty2 <- cBind(num_ThirdParty2_sparseRate1, num_ThirdParty2_sparseRate2)
num_ThirdParty <- cBind(num_ThirdParty1, num_ThirdParty2, num_ThirdParty3, num_ThirdParty4, num_ThirdParty5)
# =============================================================================
