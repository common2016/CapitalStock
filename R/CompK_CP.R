#' Compute Capital Stock in Chinese Provinces
#'
#' This function compute capital stock of provinces in China using the method by Chen (2020).
#'
#' @param prv a province name, a scalar character. It's Chinese phonetic alphabets.
#' @param startyr a numeric scalar. When use the method by Chen (2020), \code{delta} is
#' used before \code{startyr}, and after \code{startyr} depreciation in data \code{asset} is used.
#' @param yr a numeric vector about years. If you only need capital stock before 2017,
#'  you can use its default \code{NULL}. If you need to compute capital stocks in other
#'  years (for example 2018,2019), you can set, for example, \code{yr = c(2018,2019)}.
#' @param invest a numeric vector about investment, its length equal the length of
#' \code{yr}, and its units is 100 million in current price.
#' @param InvestPrice a numeric vector about price indices of investment,
#' its length equal the length of \code{yr}, and it is a fixed base index
#' with equaling 1 in \code{bt}.
#' @param depr a numeric vector about depreciation,its length equal the length of \code{yr},
#' and its units is 100 million in current price.
#' @param delta a rate of depreciation, a scalar number.

#' @param bt a scalar number, such as 2000. It means computing capital stock with its price equal
#'  1 in \code{bt}
#' @note The parameter \code{InvestPrice} is a fixed base index with equaling 1 in 1952 by default.
#' However, we often only get a price indices of investment with equaling 1
#' in last year. You can use \code{data(asset)} to get \code{InvestPrice}
#' in any year (before 2017) with equaling 1 in 1952. So, it is easy then.
#'
#' @return The function return a data.frame, and its 1st column is province, 2nd column
#'    is year, 3rd column is capital stock, 4th column is the price index of investment.
#' @references Chen, Pu, 2020, Compute capital stocks of provinces in China (In Chinese).
#' @export

CompK_CP <- function(prv,startyr = 1993, yr = NULL, invest = NULL, InvestPrice = NULL,
                     depr = NULL, delta = 0.096, bt = 1992){
  # Whether add data after 2017
  if (!is.null(yr)){
    asset <-  rbind(asset[asset$yr < min(yr),],
                    data.frame(prv = prv, yr = yr, invest = invest,
                               InvestIndex = NA, InvestPrice = InvestPrice,
                               depr = depr))
    asset <- dplyr::arrange(asset, prv, yr)
  }

  ans <- CompK_ZJ(prv = prv, yr = yr, invest = invest, InvestPrice = InvestPrice,
                  delta = delta, bt = bt)

  # depreciation of price deflator
  rawdata <- asset[asset$prv %in% prv,]
  rawdata$InvestPrice <- ans$InvestPrice # invest price deflated
  rawdata$depr <- rawdata$depr/rawdata$InvestPrice
  rawdata$invest <- rawdata$invest/rawdata$InvestPrice

  ans <- merge(ans, rawdata[,c('prv','yr','depr','invest')],all.x = T, by = c('prv','yr'))

  ans$Kcp <- NA
  # chongqing
  if (prv %in% 'chongqing' & startyr < 1996)
    stop('startyr in chongqing must larger than or equal 1996')

  ans$Kcp[ans$yr == startyr] <- ans$K[ans$yr == startyr]
  for (i in which(ans$yr == (startyr + 1)):nrow(ans)) {
    ans$Kcp[i] <- ans$Kcp[i - 1] - ans$depr[i] + ans$invest[i]
  }
  # fixed format
  ans <- ans[,c('prv','yr','Kcp','InvestPrice')]
  ans <- dplyr::rename(ans, 'K' = 'Kcp')

  return(ans)
}
