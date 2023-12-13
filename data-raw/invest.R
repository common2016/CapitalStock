## code to prepare `invest` dataset goes here
rm(list = ls())
library(pacman)
p_load(dplyr, stringr, reshape2, magrittr)
# load('data-raw/GDPInv.rdata')
asset <- openxlsx::read.xlsx('data-raw/src_data/各地区固定资本生产总额及指数.xlsx',1)
names(asset) <- c('prv','yr','FixAsset','FixAssetIndex')
asset$InvstIndex <- NA
# 处理零散的缺失值
asset$FixAssetIndex[asset$prv %in% '辽宁'] <- zoo::na.approx(asset$FixAssetIndex[asset$prv %in% '辽宁'])
asset$FixAssetIndex[asset$prv %in% '北京'] <- zoo::na.approx(asset$FixAssetIndex[asset$prv %in% '北京'])
asset$FixAsset[asset$prv %in% '北京'] <- zoo::na.approx(asset$FixAsset[asset$prv %in% '北京'])

# 处理零售价格指数，方便后面分析
rpi <- openxlsx::read.xlsx('data-raw/src_data/RPI.xlsx',1, rows = 4:35) %>%  .[,-2] # 上年=100
rpi <- melt(rpi, id.vars = '地区') %>% dcast(variable ~ 地区, value.var = 'value')
rpi$variable <- str_replace(rpi$variable, '年','') %>% as.numeric()
rpi <- dplyr::rename(rpi, yr = variable)
rpi <- arrange(rpi,yr)
# 先处理1988年以前的固定资本投资价格指数
rpi[,-1] <- rpi[,-1]/100
rpi[1,-1] <- 1

## 处理江西
CaptConstr <- openxlsx::read.xlsx('data-raw/src_data/基本建设投资江西广东.xlsx',1)
regdata <- filter(CaptConstr, yr <= 2000 & prv %in% '江西') %>%
  merge(filter(asset, prv %in% '江西' & yr >= 1979 & yr <= 2000)[,c('yr','FixAsset')], by = 'yr',all.x = T)
med <- openxlsx::read.xlsx('data-raw/src_data/固定资本形成总额2018.xlsx',1,
                           cols = c(1,15),rows = c(1,49:53),detectDate = T)
regdata$FixAsset[regdata$yr >= 1996] <- med[,2]
names(regdata)[3] <- 'constr'
regdata$FixAsset[regdata$yr <= 1978] <-
  lm(FixAsset ~ constr - 1, data = regdata) %>% predict(newdata = regdata[regdata$yr <= 1978,])
asset[asset$prv %in% '江西' & asset$yr <= 1977,'FixAsset'] <- regdata$FixAsset[regdata$yr <= 1977]

# 处理西藏
med <- openxlsx::read.xlsx('data-raw/src_data/西藏全社会固定资产投资.xlsx',1,rows = 4:5, cols = 30:43) %>% melt()
med$variable <- str_replace(med$variable,'年','') %>% as.numeric()
med <- arrange(med, variable)
asset$FixAsset[asset$prv %in% '西藏' & asset$yr >= 1978 & asset$yr <= 1991] <- med$value
med <- openxlsx::read.xlsx('data-raw/src_data/基本建设投资江西广东.xlsx',1)
asset$FixAsset[asset$prv %in% '西藏' & asset$yr <= 1977] <-
  med[med$prv %in% '西藏' & med$yr <= 1977,3]
asset$InvstIndex[asset$prv %in% '西藏' & asset$yr <= 1989] <- 1
med <- rpi[rpi$yr >= 1990,c('yr','西藏自治区')]
for (i in 2:nrow(med)) {
  med[i,2] <- med[i-1,2] * med[i,2]
}
asset$InvstIndex[asset$yr >= 1990 & asset$prv %in% '西藏'] <- med$西藏自治区[med$yr <= 1995]
medXZ <- med[med$yr >= 1996,]

# 增加广东年份
asset <- data.frame(prv = '广东', yr = 1952:1977, FixAsset = NA, FixAssetIndex = NA, InvstIndex = NA) %>%
  rbind(asset, .)
asset <- arrange(asset, prv, yr)
# 1952-1977，广东的RPI作为IPI
med <- rpi[rpi$yr <= 1978,c('yr','广东省')]
for (i in 2:nrow(med)) {
  med[i,2] <- med[i,2] * med[i-1,2]
}
asset$InvstIndex[asset$prv %in% '广东' & asset$yr >= 1952 & asset$yr <= 1978] <- med[,2]
# 直接用wind的1952-1977的固定资本形成总额替代《xx年鉴》的缺失数据
med <- openxlsx::read.xlsx('data-raw/src_data/固定资本形成总额2018.xlsx',1,
                               rows = c(1,5:30),detectDate = T, cols = c(1,20))
asset$FixAsset[asset$prv %in% '广东' & asset$yr >= 1952 & asset$yr <= 1977] <- med[,2]
# 由固定资本形成总额构造隐含的投资品价格指数(1952-1995)
for (i in unique(asset$prv)) {
  med <- asset[asset$prv %in% i,]
  if (length(med$FixAsset[med$yr == 1952]) == 0) next()
  if ( i %in% '广东'){
    med$FixAssetIndex <- med$FixAssetIndex/100
    med$InvstIndex[med$yr >= 1979] <- na.omit(med$FixAsset/(med$FixAssetIndex * med$FixAsset[med$yr == 1978]))[-1] %>%
      `*`(med$InvstIndex[med$yr == 1978])
  }else if (i %in% '西藏'){
    next()
  }else {
    med$InvstIndex <- med$FixAsset*100/(med$FixAssetIndex * med$FixAsset[med$yr == 1952])
  }
  asset[asset$prv %in% i,] <- med
}

# 处理天津的投资价格指数：1988年前用商品零售价格指数替代
med <- rpi[rpi$yr >= 1952 & rpi$yr <= 1988, c('yr','天津市')]
med$天津市 <- med$天津市 * 0.985
med$天津市[1] <- 1
for (i in 2:nrow(med)){
  med[i,-1] <- med[i,-1]*med[i-1,-1]
}
asset[asset$prv %in% '天津' & asset$yr <= 1988, 'InvstIndex'] <- med$天津市
# 再处理1989年以后的
med <- asset[asset$prv %in% '天津' & asset$yr >= 1988,]
med$FixAssetIndex[-1] <- c(0.891,0.989,1.372,1.13,1.082,1.204,1.17) # 上年=100
# 通过张军等(2004)公式4先环比，再1952=100
for (i in 2:nrow(med)) {
  med$InvstIndex[i] <- (med$FixAsset[i]/(med$FixAssetIndex[i] * med$FixAsset[i-1])) %>% `*`(med$InvstIndex[i-1])
}
asset[asset$prv %in% '天津' & asset$yr > 1988, 'InvstIndex'] <- med$InvstIndex[med$yr > 1988]

# 处理海南
asset[asset$prv %in% '海南' & asset$yr <= 1977,'InvstIndex'] <- 1
med <- rpi[rpi$yr >= 1978,c('yr','海南省')]
med$海南省[1] <- 1
for (i in 2:nrow(med)){
  med[i,-1] <- med[i,-1]*med[i-1,-1]
}
med <- rpi[rpi$yr >= 1978 & rpi$yr <= 2000,c('yr','海南省')]
med$海南省[1] <- 1
for (i in 2:nrow(med)) {
  med[i,2] <- med[i,2] * med[i-1,2]
}
medHN <- med[med$yr >= 1996,] # 留在后面延长至2000年用
asset[asset$prv %in% '海南' & asset$yr >= 1978 & asset$yr <= 1995,'InvstIndex'] <-
  med[med$yr <= 1995,2]
med <- openxlsx::read.xlsx('data-raw/src_data/固定资本形成总额2018.xlsx',1,
                    rows = c(1,31:48),detectDate = T, cols = c(1,22))
asset[asset$prv %in% '海南' & asset$yr >= 1978 & asset$yr <= 1995,'FixAsset'] <- med[,2]
med <- openxlsx::read.xlsx('data-raw/src_data/海南基本建设投资.xlsx',1)
asset[asset$prv %in% '海南' & asset$yr >= 1952 & asset$yr <= 1977,'FixAsset'] <-
  med[med$yr >= 1952 & med$yr <= 1977,2]



# 加入1995年以后的数据: 此时投资指数用固定资产投资价格指数
asset18 <- openxlsx::read.xlsx('data-raw/src_data/固定资本形成总额2018.xlsx',1,rows = c(1,49:70),detectDate = T)
asset18 <- melt(asset18, id.vars = '指标名称')
asset18$prv <- str_split_fixed(asset18$variable,':',3) %>% .[,1]
asset18$yr <- format(asset18$指标名称,'%Y') %>% as.numeric()
asset18 <- dplyr::rename(asset18, FixAsset = value)

InvstIndex <- openxlsx::read.xlsx('data-raw/src_data/固定资产投资价格指数2018.xlsx',1,rows = c(1,15:37),detectDate = T)
InvstIndex <- melt(InvstIndex, id.vars = '指标名称')
InvstIndex$prv <- str_split_fixed(InvstIndex$variable,':',2) %>% .[,2]
InvstIndex$yr <- format(InvstIndex$指标名称,'%Y') %>% as.numeric()
InvstIndex <- dplyr::rename(InvstIndex, InvstIndex = value)
InvstIndex$InvstIndex <- InvstIndex$InvstIndex/100

asset18 <- merge(asset18[,c('prv','yr','FixAsset')], InvstIndex[,c('prv','yr','InvstIndex')],
                 by = c('prv','yr'), all.x = T)
asset18 <- asset[asset$yr == 1995,c('prv','yr','FixAsset','InvstIndex')] %>% rbind(asset18,.)

# 四川1995作为重庆1995
med <- asset[asset$prv %in% '四川' & asset$yr == 1995,c('prv','yr','FixAsset','InvstIndex')]
med[1,1] <- '重庆'
med[1,3] <- 0
asset18 <- rbind(asset18, med)
asset18 <- arrange(asset18,prv,yr)
asset18[asset18$prv %in% '广东' & asset18$yr >= 1996 & asset18$yr <= 2000,'InvstIndex'] <-
  asset18[asset18$prv %in% '福建' & asset18$yr >= 1996 & asset18$yr <= 2000,'InvstIndex']
for (i in unique(asset18$prv)) {
  med <- asset18[asset18$prv %in% i,]
  for (j in 2:nrow(med)){
    med$InvstIndex[j] <- med$InvstIndex[j-1] * med$InvstIndex[j]
  }
  asset18[asset18$prv %in% i,] <- med
}
asset18[asset18$prv %in% '海南' & asset18$yr >= 1996 & asset18$yr <= 2000,'InvstIndex'] <- medHN[,2]
asset18[asset18$prv %in% '西藏' & asset18$yr >= 1996 & asset18$yr <= 2017,'InvstIndex'] <- medXZ[medXZ$yr <= 2017,2]
asset18$FixAssetIndex <- NA
asset18 <- asset18[,names(asset)]
asset <- rbind(asset,asset18[asset18$yr != 1995,])

names(asset)[3:5] <- c('invest','InvestIndex','InvestPrice')
# 改汉语拼音
ans <- openxlsx::read.xlsx('data-raw/src_data/各地区固定资本生产总额及指数.xlsx',2)
asset <- merge(asset, ans, by = 'prv', all.x = T) %>%
  dplyr::select(alphabets,yr,invest,InvestIndex, InvestPrice) %>%
  dplyr::rename(prv = alphabets)
asset <- arrange(asset,prv,yr)

# 将各省每年折旧数据添加进0.0.2版本的asset
depr <- openxlsx::read.xlsx('data-raw/src_data/固定资产折旧.xlsx',1,detectDates = T)
depr <- melt(depr,id.vars = '指标名称',value.name = 'depr',variable.name = 'prv')
depr$prv <- str_split_fixed(depr$prv,':',3) %>% .[,1]
depr$yr <- format(depr$指标名称,'%Y')
ans <- openxlsx::read.xlsx('data-raw/src_data/各地区固定资本生产总额及指数.xlsx',2)
prv <- ans
depr <- merge(depr, ans, by = 'prv', all.x = T) %>% dplyr::select(-prv,-指标名称) %>%
  dplyr::rename(prv = alphabets)
# data("asset")
asset <- merge(asset, depr, by = c('prv','yr'), all.x = T)

# save(asset, file = 'data-raw/rlt_data/asset2017.rdata')

# 2021年完成
