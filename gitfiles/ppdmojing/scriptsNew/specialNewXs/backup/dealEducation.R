# 提取教育信息 ------------------------------------------------------
cat_dataMaster <- readRDS("dataNew/cat_dataMaster")
cat_Education <- cat_dataMaster[, grepl("^Education", names(cat_dataMaster))]
# ===================================================================
# 标记"E"和"不详"为空值
cat_Education[cat_Education == "E"] <- NA
cat_Education[cat_Education == "不详"] <- NA
# ===================================================================
# 对dataEducation做one hot转换，并标记空值位置 ----------------------
cat_Education_onehot <- lapply(cat_Education, FUN = one_hot)
cat_Education_onehot <- do.call("cBind", cat_Education_onehot)
names(cat_Education_onehot) <- paste0("Education_onehot_", names(cat_Education_onehot))
cat_Education_sparse <- mark_sparseValue(data = cat_Education)
names(cat_Education_sparse) <- paste0("Education_sparse_", names(cat_Education_sparse))
cat_Education <- cBind(cat_Education_onehot, cat_Education_sparse)
# ===================================================================
