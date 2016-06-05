library(Matrix)
library(stringr)
library(data.table)
library(xlsx)
library(snow)

# 辅助函数 -----------------------------------------------------------------
analytics_byRow <- function(data = NULL, f = NULL){
  data <- do.call("cBind", data)
  data <- apply(X = data, MARGIN = 1, FUN = f)
  data <- data.frame(data)
}
count_theCategory <- function(data = NULL){
  if(is.data.frame(data))  table <- table(unlist(data))
  else if(is.matrix(data)) table <- table(c(data))
  for(k in 1:ncol(data)){
    names <- names(table)
    Map(f = function(x1, x2){
      data[grepl(paste0("^", x1, "$"), data[, k]), k] <<- x2
    }, names, table)
  }
  data <- lapply(data, as.numeric)
  data.frame(data, stringsAsFactors = F)
}
mark_sparseValue <- function(data = NULL){
  data[!is.na(data)] <- 0
  data[is.na(data)] <- 1
  data
}
one_hot <- function(x){
  vector <- x
  if(all(is.na(x))) return(data.frame(rep(0, length(x)))) else{
    levels <- levels(factor(x))
    dummy <- lapply(levels,function(x){
      out <- rep(x,length(vector)) == as.character(vector)
      out[is.na(out)] <- 0
      out
    })
    names(dummy) <- levels
    return(data.frame(dummy))
  }
}
one_count <- function(x){
  vector <- str_sub(x, start = 1, end = 1)
  levels <- levels(factor(vector))
  dummy <- lapply(levels,function(x){
    out <- rep(x,length(vector)) == as.character(vector)
    out[is.na(out)] <- 0
    out
  })
  dummy <- data.frame(dummy, stringsAsFactors = F)
  data <- as.data.frame(rep(data.frame(x, stringsAsFactors = F), ncol(dummy)), stringsAsFactors = F)
  data[dummy == 0] <- NA
  count_data <- count_theCategory(data)
  count_data[is.na(count_data)] <- 0
  return(count_data)
}
cross_category <- function(x, y){
  paste0(unlist(x), y)
}
cluster_Topk <- function(data = NULL, id = NULL, cluster = NULL){
  data[is.na(data)] <- 0
  frequency <- vapply(data, sum, numeric(1))
  frequency_name <- names(frequency)[order(frequency, decreasing = T)]
  data_Topk <- data[, frequency_name[id], drop = F]
  data.frame(kmeans(data_Topk, centers = cluster)$cluster, stringsAsFactors = F)
}
map_woe <- function(x = NULL, target = NULL, Threshold = NULL){
  table_x <- table(x)
  table_xAndtarget <- table(data.frame(x, target))
  table_xAndtarget[names(table_x[table_x < Threshold]), "1"] <- NA
  out <- table_xAndtarget[as.character(x), "1"] / table_x[as.character(x)]
  names(out) <- NULL
  return(out)
}
auto_char <- function(x = NULL, target = NULL){
  data <- data.frame(x = x, target = target, stringsAsFactors = F)
  data[["x"]] <- str_trim(tolower(data[["x"]]))
  data[["x"]][is.na(data[["x"]])] <- "NA"
  char <- names(table(data[["x"]]))
  p.all <- sum(data[["target"]] == 1) / length(data[["target"]])
  charList <- lapply(char, function(x) data[data[["x"]] %in% x,])#研究一下subset的机制
  tableList <- lapply(charList, function(x){
    count_1 <- sum(x[["target"]] == 1)
    count_all <- length(x[["target"]])
    binom.p <- binom.test(count_1, n = count_all, p = p.all)$p.value
    data.frame(数量 = count_all, binom.p = binom.p, stringsAsFactors = F)
  })
  score <- cBind(名称 = char, do.call("rBind", tableList), stringsAsFactors = F)
  subset(score, (数量 >= 5000 & binom.p >= 0.2) | (数量 >= 1000 & 数量 < 5000 & binom.p > 0.05) | (数量 >= 500 & 数量 < 1000 & binom.p > 0.01) | (数量 >= 100 & 数量 < 500 & binom.p > 0.001) | 数量 < 100)$名称
}
statByTime <- function(data = NULL, breaks = NULL, Fun = NULL, name = NULL, nameFun = NULL){
  stopifnot(breaks != 0 & ncol(data) %% breaks == 0)
  num_col <- as.integer(ncol(data) / breaks)
  dataList <- lapply(1:breaks, function(k){
    data <- data[, (k -1) * num_col + 1:num_col]
    attr(data, "WhichPart") <- k
    return(data)
    })
  statList <- lapply(dataList, function(df){
    List <- lapply(Fun, function(f0) analytics_byRow(df, f = function(x_row) ifelse(all(is.na(x_row)), NA, f0(x_row, na.rm = T))))
    data <- do.call("cBind", List)
    names(data) <- paste0(name, "_breaks", breaks, "_", attr(df, "WhichPart"), "_", nameFun)
    return(data)
  })
  do.call("cBind", statList)
}
caculate_slide <- function(x, ...){
  time <- 1:length(x)
  lm(x ~ time)$coefficients[2]
}
# ==========================================================================