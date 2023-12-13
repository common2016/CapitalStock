# 更新数据至2022年

# 2017年以后无分省固定资本形成总额，若把固定资产投资完成额同比当做
# 固定资本形成总额的同比，则可使用固定资产投资完成额同比数据以及2017年
# 固定资本形成总额数据，得到固定资本形成总额的2018-2022数据

rm(list = ls())
library(tidyverse)
invest23 <- readxl::read_xlsx('data-raw/src_data/2023分省固定资产投资完成额同比.xlsx',1,range = 'R1C2:R32C39') |>
  select(-频率, -单位, -时间区间, -来源, - 更新时间, -指标ID) # 读取同比数据
names(invest23)[-1] <- as.character(2022:1992)
invest23$指标名称 <- str_split_fixed(invest23$指标名称, ':', n = 3)[,1]
invest23 <- pivot_longer(invest23, -指标名称, names_to = 'yr', values_to = 'TB') |>
  rename(prv = 指标名称) |> filter(yr >= 2017)
invest23$yr <- as.numeric(invest23$yr)

# 提取2017年资本形成总额
invest17 <- openxlsx::read.xlsx('data-raw/src_data/固定资本形成总额2018.xlsx',1,rows = c(1,49:70),detectDate = T) |>
  pivot_longer(-指标名称, values_to = 'invest', names_to = 'prv')
invest17$指标名称 <- format(invest17$指标名称,'%Y') |> as.numeric()
invest17$prv <- str_split_fixed(invest17$prv, ':', 3)[,1]
invest17 <- filter(invest17, 指标名称 == 2017) |> rename(yr = 指标名称)

invest23 <- left_join(invest23, invest17, by = join_by('prv','yr')) |> arrange(prv, yr)
# 利用同比数据获得固定资产投资完成额
for (i in 2:nrow(invest23)) {
  if (invest23$prv[i] == invest23$prv[i-1]){
    invest23$invest[i] <- invest23$invest[i-1]*(1+invest23$TB[i]/100)
  }
}

# 固定资产投资价格指数只更新至2019年，故2020年以后固定资产投资价格指数用零售价格指数替代（西藏一直都是这么替代）
inv_pri <- readxl::read_xlsx('data-raw/src_data/固定资产投资价格指数2019.xlsx',1, range = 'A1:L31') |>
  select(c(2,9:12))
names(inv_pri) <- c('prv', as.character(2019:2016))
inv_pri$prv <- str_split_fixed(inv_pri$prv, ':', 3)[,1]
inv_pri <- pivot_longer(inv_pri, -prv, names_to = 'yr', values_to = 'InvestPrice') |>
  arrange(prv, yr) |> mutate(yr = as.numeric(yr), InvestPrice = InvestPrice/100)

asset23 <- left_join(select(invest23, -TB), inv_pri, by = join_by('prv','yr'))

# RPI填补固定资产投资价格指数缺失值
rpi <- readxl::read_xlsx('data-raw/src_data/2022RPI.xlsx',1,range = 'A1:N32') |>
  select(-国家,-频率, -单位, -指标ID, -时间区间, -来源, -更新时间)
names(rpi) <- c('prv',as.character(2022:2017))
rpi$prv <- str_split_fixed(rpi$prv, ':', 2)[,1]

for (prv in unique(asset23$prv)) {
  for (yr in 2018:2022) {
    if (is.na(asset23$InvestPrice[asset23$prv %in% prv & asset23$yr == yr])){
      asset23$InvestPrice[asset23$prv %in% prv & asset23$yr == yr] <-
        (rpi[rpi$prv %in% prv, as.character(yr)]/100) |> as.numeric()
    }
  }
}

# 改拼音
ans <- openxlsx::read.xlsx('data-raw/src_data/各地区固定资本生产总额及指数.xlsx',2)
asset23 <- left_join(asset23, ans, by = join_by('prv')) |> select(-prv) |>
  rename(prv = alphabets) |> relocate(prv)

# 获取2017年定基投资价格指数, 并连乘获得2018-2022的定基比数据
load('data-raw/rlt_data/asset2017.rdata')
for (i in unique(asset23$prv)) {
  asset23$InvestPrice[asset23$prv %in% i & asset23$yr == 2017] <- asset$InvestPrice[asset$prv %in% i & asset$yr == 2017] # 得到2017年
  for (yr in 2018:2022) {
    asset23$InvestPrice[asset23$prv %in% i & asset23$yr == yr] <- asset23$InvestPrice[asset23$prv %in% i & asset23$yr == yr]*
      asset23$InvestPrice[asset23$prv %in% i & asset23$yr == yr-1]
  }
}

# 与2021年的数据合并
asset23 <- mutate(asset23, InvestIndex = NA, depr = NA) |> relocate(prv, yr, invest, InvestIndex, InvestPrice, depr) |>
  filter(yr >= 2018)
asset <- rbind(asset, asset23) |> arrange(prv, yr)
# usethis::use_data(asset, overwrite = TRUE)
# ---------2023年完成----------
