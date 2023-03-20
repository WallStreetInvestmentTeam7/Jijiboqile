library(rvest)
library(TTR)
library(xts)
#library(WindR)
## 启动WindR


## 下载数据
code <- '600030.SH' #代码
fields <- 'open,high,low,close,volume,adjfactor' # 数据变量名
datestart <- '2003-01-06' # 起始时间
w_wsd_data <- w.wsd(codes = code, fields = fields, startdate = datestart)

## 去掉股票停牌期间的数据
data_temp <- w_wsd_data$Data[which(w_wsd_data$Data$VOLUME > 0), ]

## 股票价格向后复权
data_temp$OPEN  <- data_temp$OPEN  * data_temp$ADJFACTOR  # 开盘价
data_temp$HIGH  <- data_temp$HIGH  * data_temp$ADJFACTOR  # 最高价
data_temp$LOW   <- data_temp$LOW   * data_temp$ADJFACTOR  # 最低价
data_temp$CLOSE <- data_temp$CLOSE * data_temp$ADJFACTOR  # 收盘价
## 将数据转化为xts格式
stock_use <- xts(data_temp[, 2:7], order.by = as.Date(data_temp[, 1]))
## 数据表KDJ用来存放最终的数据
l_temp <- nrow(stock_use)         # 数据长度
KDJ <- matrix(NA, l_temp, 3)      # 构建存放数据的矩阵
KDJ <- as.data.frame(KDJ)         # 转换为data.frame
colnames(KDJ) <- c('K', 'D', 'J') # 1-3列的名称分别命名为K、D、J
KDJ[1:8, ]  <- 50                 # 前8天的K、D、J均设为50
## 计算rvs
# 计算9日内最高价
high_9 <- lag.xts(stock_use$HIGH, k = c(0:8))
high_max <- apply(high_9, MARGIN = 1, FUN = max)
# 计算9日内最低价
low_9 <- lag.xts(stock_use$LOW, k = c(0:8))
low_min <- apply(low_9, MARGIN = 1, FUN = min)
# rsv
rsv <- (stock_use$CLOSE - low_min) / (high_max - low_min) * 100
## 计算KDJ
for (i in 9:l_temp) {
  # 计算K值
  KDJ[i, 1] <- 2/3 * KDJ[(i - 1), 1] + 1/3 * rsv[i, ]
  # 计算D值
  KDJ[i, 2] <- 2/3 * KDJ[(i - 1), 2] + 1/3 * KDJ[i, 1]
  # 计算J值
  KDJ[i, 3] <- 3 * KDJ[i, 1] - 2 * KDJ[i, 2]
}
## 将KDJ转化为xts格式
KDJ <- as.xts(KDJ, order.by = index(rsv))