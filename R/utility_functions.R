#' Biexponential transformation (inspired by flowJo)
#'
#' @export
#' @import scales
biexp_trans <- function(lim = 5, decade.size = lim){
  trans <- function(x){
    ifelse(x <= lim,
           x,
           lim + decade.size * (suppressWarnings(log(x, 10)) -
                                  log(lim, 10)))
  }
  inv <- function(x) {
    ifelse(x <= lim,
           x,
           10^(((x-lim)/decade.size) + log(lim,10)))
  }
  breaks <- function(x) {
    if (all(x <= lim)) {
      scales::pretty_breaks()(x)
    } else if (all(x > lim)) {
      scales::breaks_log(5)(x)
    } else {
      unique(c(scales::pretty_breaks()(c(x[1],lim)),
               scales::breaks_log(5)(c(lim, x[2]))))
    }
  }
  scales::trans_new(paste0("biexp-",format(lim)), trans, inv, breaks)
}

#' Robust Standard Deviation
#'
#' @param x vector of values
#' @param p quantile probabilities.
#' @return Standard deviation within selected quantiles = Robust SD
#'
#' @export
sd_robust <- function(x, p=c(0.1, 0.9)){
  quant <- quantile(x, probs=p)

  x_robust <- x[x >= quant[1] & x <= quant[2]]

  return(sd(x_robust))
}

#' Calculate Staining Index
#'
#' ((MEDpos - MEDneg) / 2*SD(neg))
#'
#' @param positive Vector of values for positive cells
#' @param negative Vector of values for negative cells
#' @param SD_FUN Function for calculating Standard Deviation

staining_index <- function(positive, negative, SD_FUN=sd_robust){
  diff <- (median(positive)-median(negative))
  spread <- (2*SD_FUN(negative))

  ## If negative population has all 0 values, SI will be infinite. To avoid this we do not allow 2*SD to be below 1.
  # Can this be done more elegantly?
  if(spread < 1) spread <- 1

  SI <- diff/spread

  return(SI)
}

#' Calculate Staining Index per Thousand (SIPK)
#'
#' SI / (TOTAL/1000)
#'
#' @param positive Vector of values for positive cells
#' @param negative Vector of values for negative cells
#' @param total Total value. If NULL, then total will be calculated as the sum of the positive and negative vectors
#' @param denominator Denominator for total value
#' @param ... Passed on to staining_index()
#' @export
sipk <- function(positive, negative, total=NULL, denominator=1000, ...){

  if(!is.null(total)) total <- sum(positive) + sum(negative)

  sipk <- staining_index(positive, negative, ...)/(total/denominator)

  return(sipk)
}
