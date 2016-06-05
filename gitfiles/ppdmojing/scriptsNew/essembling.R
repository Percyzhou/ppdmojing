# 设定小数点位数 ------------------
options(digits = 4)
# =================================
preds.dt779 <- read.csv("score/predict779.csv", stringsAsFactors = F)
names(preds.dt779) <- c("Idx", "preds.dt779")
preds.dt778 <- read.csv("score/final0.778.csv", stringsAsFactors = F)
names(preds.dt778) <- c("Idx", "preds.dt778")
preds.dt782 <- read.csv("score/final0.782.csv", stringsAsFactors = F)
names(preds.dt782) <- c("Idx", "preds.dt782")
preds.lr <- read.csv("score/predict75012csv", stringsAsFactors = F)
names(preds.lr) <- c("Idx", "preds.lr")

preds.table <- Reduce(f = function(x, y) merge(x, y, by = "Idx", all = F), list(preds.dt779, preds.dt778, preds.dt782, preds.lr))
Idx <- preds.table[["Idx"]]
preds.table$Idx <- NULL
preds.table[] <- lapply(preds.table, rank)
score <- with(preds.table, 1.5 * preds.dt779 + 1 * preds.dt778 + 2 * preds.dt782 + 0.05 * preds.lr)/1000
preds.final <- cBind(Idx = Idx, score = score)
write.csv(preds.final, "score/predictFinal.csv", row.names = F, fileEncoding = "utf-8")