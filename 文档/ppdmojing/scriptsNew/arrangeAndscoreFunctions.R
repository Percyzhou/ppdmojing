arrangeChar <- function(x = NULL, charLists = NULL){
  for(k in 1:length(charLists)){
    x[x %in% charLists[[k]]] <- Reduce(f = function(x, y) paste0(x, "/", y), charLists[[k]])
  }
  return(x)
}
replaceChar <- function(x = NULL, char = NULL, replaceChar = NULL){
  stopifnot(length(char) == length(replaceChar))
  if(length(char) != 0){
    for(k in 1:length(char)){
      if(is.na(char[k])){
        x[is.na(x)] <- replaceChar[k]
      } else x[x == char[k]] <- replaceChar[k]
    }
  }
  return(x)
}

replaceChar2 <- function(x = NULL, char = NULL, replaceChar = NULL){
  stopifnot(length(char) == length(replaceChar))
  if(length(char) != 0){
    for(k in 1:length(char)){
      if(is.na(char[k])){
        x[is.na(x)] <- replaceChar[k]
      } else x[x %in% char[[k]]] <- replaceChar[[k]]
    }
  }
  return(x)
}
scoreChar <- function(data = NULL, x_name = NULL, y_name = NULL){
  data <- data[, c(x_name, y_name)]
  data[[x_name]] <- str_trim(tolower(data[[x_name]]))
  data[[x_name]][is.na(data[[x_name]])] <- "NA"
  char <- names(table(data[[x_name]]))
  p.all <- sum(data[[y_name]] == 1) / length(data[[y_name]])
  charList <- lapply(char, function(x) data[data[[x_name]] %in% x,])#研究一下subset的机制
  tableList <- lapply(charList, function(x){
    count_1 <- sum(x[[y_name]] == 1)
    count_all <- length(x[[y_name]])
    rate_1 <- count_1 / count_all
    binom.p <- binom.test(count_1, n = count_all, p = p.all)$p.value
    data.frame(数量 = count_all, 违约数量 = count_1, 违约率 = rate_1, binom.p = binom.p, stringsAsFactors = F)
  })
  cBind(名称 = char, do.call("rBind", tableList))
}
scoreNum <- function(data = NULL, x_name = NULL, y_name = NULL){
  data <- data[, c(x_name, y_name)]
  data[[x_name]] <- str_trim(tolower(data[[x_name]]))
  char <- c("-1", "0")
  p.all <- sum(data[[y_name]] == 1) / length(data[[y_name]])
  charList <- lapply(char, function(x) data[data[[x_name]] %in% x,])#研究一下subset的机制
  tableList <- lapply(charList, function(x){
    count_1 <- sum(x[[y_name]] == 1)
    count_all <- length(x[[y_name]])
    rate_1 <- count_1 / count_all
    binom.p <- binom.test(count_1, n = count_all, p = p.all)$p.value
    data.frame(数量 = count_all, 违约数量 = count_1, 违约率 = rate_1, binom.p = binom.p, stringsAsFactors = F)
  })
  cBind(名称 = char, do.call("rBind", tableList))
}
scoreNum2 <- function(data = NULL, x_name = NULL, y_name = NULL, breaks = 10, missing = -1){
  x <- data[[x_name]]
  x[x == missing] <- NA
  x <- rank(x, na.last = "keep")
  data[[x_name]] <- cut(x, breaks = breaks, labels = 1:breaks)
  scoreChar(data, x_name = x_name, y_name = y_name)
}