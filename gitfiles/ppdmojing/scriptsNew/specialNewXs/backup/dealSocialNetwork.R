# 提取SocialNetwork子集 -----------------------------------------------------
num_dataMaster <- readRDS("dataNew/num_dataMaster")
cat_dataMaster <- readRDS("dataNew/cat_dataMaster")

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

num_SocialNetwork <- cBind(num_SocialNetwork, num_SocialNetwork_min, num_SocialNetwork_max, num_SocialNetwork_sum, num_SocialNetwork_sparseRate)
# ===========================================================================
# 提取多种衍生变量 ----------------------------------------------------------
cat_SocialNetwork <- lapply(cat_SocialNetwork, one_hot)
cat_SocialNetwork <- do.call("cBind", cat_SocialNetwork)
names(cat_SocialNetwork) <- paste0("SocialNetwork_onehot_", names(cat_SocialNetwork))
# ===========================================================================
# 组合SocialNetwork数据 -----------------------------------------------------
dataMaster_SocialNetwork <- cBind(num_SocialNetwork, cat_SocialNetwork)
# ===========================================================================