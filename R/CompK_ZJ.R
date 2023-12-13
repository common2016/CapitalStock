#' Compute Capital Stock in Chinese Provinces
#'
#' This function compute capital stock of provinces in China using the method by Zhang (2008).
#'
#' @param yr a numeric vector about years. If you only need capital stock before 2017,
#'  you can use its default \code{NULL}. If you need to compute capital stocks in other
#'  years (for example 2018,2019), you can set, for example, \code{yr = c(2018,2019)}.
#' @param invest a numeric vector about investment, its length equal the length of
#' \code{yr}, and its units is 100 million in current price.
#' @param InvestPrice a numeric vector about price indices of investment,
#' its length equal the length of \code{yr}, and it is a fixed base index
#' with equaling 1 in \code{bt}.
#' @param delta a rate of depreciation, a scalar number.
#' @param prv a province name, a scalar character. It's Chinese phonetic alphabets.
#' @param bt a scalar number, such as 2000. It means computing capital stock with its price equal
#'  1 in \code{bt}
#' @note The parameter \code{InvestPrice} is a fixed base index with equaling 1 in 1952 by default.
#' However, we often only get a price indices of investment with equaling 1
#' in last year. You can use \code{data(asset)} to get \code{InvestPrice}
#' in any year (before 2017) with equaling 1 in 1952. So, it is easy then.
#'
#' @return The function return a data.frame, and its 1st column is province, 2nd column
#'    is year, 3rd column is capital stock, 4th column is the price index of investment.
#' @references Zhang, J., Estimation of China's provincial capital stock (1952-2004) with
#' applications. \emph{Journal of Chinese Economic and Business Studies}, 2008. 6(2): p. 177-196.
#' @export


CompK_ZJ <- function(yr = NULL, invest = NULL, InvestPrice = NULL,
                  delta = 0.096, prv, bt = 1952){
  # Whether add data after 2017
  if (!is.null(yr)){
    asset <-  rbind(dplyr::select(asset[asset$yr < min(yr),],-'depr'),
                    data.frame(prv = prv, yr = yr, invest = invest,
                               InvestIndex = NA, InvestPrice = InvestPrice))
    asset <- dplyr::arrange(asset, prv, yr)
  }

  K <- asset[asset$yr == 1952,c('prv','yr','invest')]
  K$K <- K$invest/0.1
  asset <- merge(asset, K[,c('prv','yr','K')], by = c('prv','yr'), all.x = T)

  ans <- asset[asset$prv %in% prv,]

  if (prv %in% 'chongqing') {
    # modify base time
    ifelse (bt < 1996,
            ans$InvestPrice <- ans$InvestPrice/asset$InvestPrice[asset$yr == bt & asset$prv %in% 'sichuan'],
            ans$InvestPrice <- ans$InvestPrice/ans$InvestPrice[ans$yr == bt])
    ans$RealInvest <- ans$invest/ans$InvestPrice
    ans$K[1] <- 1090*313/850
  }else {
    ans$InvestPrice <- ans$InvestPrice/ans$InvestPrice[ans$yr == bt]
    ans$RealInvest <- ans$invest/ans$InvestPrice
  }

  for (i in 2:nrow(ans)) {
    ans$K[i] <- ans$K[i-1] * (1-delta) + ans$RealInvest[i]
  }
  ans <- ans[,c('prv','yr','K','InvestPrice')]
  class(ans) <- c('data.frame','CapStk')
  return(ans)
}
