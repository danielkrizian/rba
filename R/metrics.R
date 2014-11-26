metric_label <- function(FUN, limit) {
  #TODO: limit not yet implemented
  
  metric.labels = c(sd="Vol", 
                    performance="Return")
  label = as.character(metric.labels[FUN][1])
  if(is.na(label))
    label = FUN
  return(label)
}




annualized <- function(x) {
  structure(x, name=paste0("Ann. ", attr(x, "name")), class=c("annualized", class(x)))
}

trailing <- function(x, ...) UseMethod("trailing")

trailing.default <- function(x, period) {
  l = index(x)[length(index(x))]
  out = switch(tolower(period), 
               "mtd" = x[paste(as.Date(cut(l,"month")) - 1, "::")],
               "qtd" = x[paste(as.Date(cut(l,"quarter")) - 1, "::")],
               "ytd" = x[paste(as.Date(cut(l,"year")) - 1, "::")],
               "l12m"= x[paste(as.Date(l) - months(12), "::")],
               xts:::last.xts(x, n=period))
  structure(out, name=paste0(period, ifelse(is.null(attr(x, "name")), "", attr(x, "name"))),
            class=c("trailing", class(out)))
  #TODO: xts:::last.xts(x, "3 weeks") doesn't work for monthly data. Employ own
  #version of last
}

# TODO: make calculate dispatch on both string and symbol representation of function
#   if(is.character(substitute(FUN)))
#     if(existsFunction(FUN))
#       FUN <- match.fun(FUN)
calculate <- function(x, FUN, ...) {
  UseMethod("calculate", x)
}

# returns single metric for each column. Takes care of leading NAs
calculate.default = function(x, FUN, ...) {
  FUNC = match.fun(FUN)
  out = vapply(x, function(col, ...) {
    FUNC(col[xts:::naCheck(col)$nonNA], ...)
  }, FUN.VALUE = numeric(1), ...=...)
  structure(out, name = metric_label(FUN))
}


calculate.rolling <- function(x, FUN, ...) {
  xtsrunapply(x, FUN, ...)
} 

calculate.annualized = function(x, FUN, ...) {
  FUN.ann <- paste(FUN, "ann", sep=".")
  FUNC = if(existsFunction(FUN.ann)) match.fun(FUN.ann) else match.fun(FUN)
  ann = xtsAttributes(x)$ann
  if(existsFunction(FUN.ann))
    out = vapply(x, function(col, ...) {
      FUNC(col[xts:::naCheck(col)$nonNA], ann=ann, ...)
    }, FUN.VALUE = numeric(1), ...=...)
  else {
    out = vapply(x, function(col, ...) {
      FUNC(col[xts:::naCheck(col)$nonNA], ...)
    }, FUN.VALUE = numeric(1), ...=...)
    out = ann * out
  }
  out = list(out)
  names(out) = paste("Ann.", metric_label(FUN))
  return(out)
}

calculate.trailing = function(x, FUN, ...) {
  out = calculate.default(x, FUN, ...)
  out = list(out)
  names(out) = paste(attr(x, "name"), names(out))
  out
}


#### PERFORMANCE ####

# cagr <- function(value, n, base=100, ann=252) {
#   (value/base)^(ann / n) - 1
# }
# 
# twr <- function(value, base=100) {
#   value/base
# }

compound <- function(x) prod(1+x)

performance <- function(x) {
  compound(x) - 1
}

performance.ann <- function(x, ann) {
  compound(x)^(ann/length(x)) - 1
}

#### PERFORMANCE RELATIVE (CAPM) ####

capm <- function(x, ...) UseMethod("capm")

capm.returns <- function(x, Rf=0) {
  R = na.omit(x) - Rf
  benchmarks = xtsAttributes(x)$benchmarks
  assets = setdiff(colnames(x), benchmarks)
  
  out = sapply(benchmarks, function(benchmark){
    browser()
    bpos = pmatch(benchmark, colnames(R))
    apos = pmatch(assets, colnames(R))
    model.lm = lm(R[, apos] ~ R[, bpos], R)
    alpha = coef(model.lm)[[1]]
    beta = coef(model.lm)[[2]]
    list(Alpha=alpha, Beta=beta)
  })
  
  list(Alpha=unlist(out["Alpha",]), beta=unlist(out["Beta",]))
}

##### VOLATILITY ####################

sd.ann <- function(x, ann, ...) sqrt(ann) * sd(x, ...)


tr <- truerange <- TR <- function(hi, lo, cl) {
  clag = c(NA, cl[-length(cl)])
  trueHi = pmax(hi, clag, na.rm = FALSE)
  trueLo = pmin(lo, clag, na.rm = FALSE)
  trueHi - trueLo
}

#' @import TTR
atr <- ATR <- function(hi, lo, cl, n=14, ma, ...) {
  tr = TR(hi, lo, cl)
  maArgs <- list(n = n, ...)
  if (missing(ma)) {
    ma <- "EMA"
    maArgs$wilder <- TRUE
  }
  do.call(ma, c(list(tr), maArgs))
}

##### RISK-ADJUSTED PERFORMANCE #####

sharpe <- function(R, Rf=0,ann=252) {
  excess = R - Rf
  sqrt(ann) * mean(excess) / sd(excess)
}

#' Some Title
#'
#' @param equity
#' @export
r2 <- function(equity) {
  cor(equity, 1:length(equity))^2
}

#' David Varadi's Ratio
#'
#' @export
dvr <- function(R, Rf=0, ann=252) {
  sharpe(R, Rf=Rf, ann=ann) * r2(cumprod(1+R))
}

# http://www.investopedia.com/terms/m/mar-ratio.asp
mar <- function(R, ann=252) {
  e =  cumprod(1+R)
  n =length(e)
  dd = abs(e / cummax(e) - 1)
  cagr(e[n], n, base=1, ann) / max(dd)
}

####### DRAWDOWNS ######

# Calculate drawdowns
dd <- function(x) {
  e <- value.index(x)
  e / cummax(e) - 1
}

maxdd <- function(x) {
  dd <- dd(x)
  max(abs(dd))
}

# Average drawdown length
avgdd <- function(x) {
  dd <- dd(x)
  prevdd <- c(0, dd[-length(dd)])

  ddstarts = which( dd != 0 & prevdd == 0 )
  ddends = which( dd == 0 & prevdd != 0 )

  if(tail(ddends,1)!=length(dd))
    ddends <- c(ddends, length(dd)) # close last incomplete drawdown

  if(length(ddends) != length(ddstarts)) {
    cat(dd)
    stop(paste("Error calculating average drawdown. There are", length(ddstarts), "DD starts and ", length(ddends), "DD ends"))
  }

  abs(mean(apply( cbind(ddstarts, ddends), 1, function(x){ min( dd[ x[1]:x[2] ]) } )))
}

summary.drawdowns <- function(drawdowns, dates=NULL) {

  prevdd <- c(0, drawdowns[-length(drawdowns)])

  ddstarts = which( drawdowns != 0 & prevdd == 0 )
  ddends = which( drawdowns == 0 & prevdd != 0 )
  if(!length(ddstarts) | !length(ddends))
    return(data.table(From=as.Date(character(0))
                      ,Trough=as.Date(character(0))
                      , To=as.Date(character(0))
                      , Depth=numeric(0)
                      ,  Length= numeric(0)
                      ,"To Trough"=numeric(0)
                      ,"Recovery"=numeric(0)
                      , key="Depth"))

  if(tail(ddends,1)!=length(drawdowns) & drawdowns[length(drawdowns)] !=0)
    ddends <- c(ddends, length(drawdowns)) # close last incomplete drawdown

  ddthroughs <- rbindlist(sapply(1:length(ddstarts), function(x) {
    ddsubset <- drawdowns[ddstarts[x]:ddends[x]]
    depth <- min(ddsubset)
    list(depth=depth,
         index=which(drawdowns==depth)[1])  # take first if multiple matches
  }, simplify=FALSE))

  out <- data.table(From=dates[ddstarts]
                    ,Trough=dates[ddthroughs$index]
                    , To=dates[ddends]
                    , Depth=ddthroughs$depth
                    ,  Length= ddends - (ddstarts - 1)
                    ,"To Trough"=ddthroughs$index - (ddstarts - 1)
                    ,"Recovery"=ddends - ddthroughs$index
                    , key="Depth")
  out[order(Depth)]
}

######## TRADES #######

avgwin <- function(x, extreme=F) {
  wins <- x[x>0]
  if(!length(wins))
    return(as.numeric(NA))
  if(!extreme)
    wins <- wins[which(wins<max(wins))]
  mean(wins)
}

avgloss <- function(x, scratch=F) {
  mean(x[x<0])
}

winrate <- function(x) {
  sum(x>0)/length(x)
}

winloss <- function(x, extreme=F) {
 avgwin(x, extreme=extreme)/abs(avgloss(x))
}

expectancy <- function(x) {
#   http://www.learningmarkets.com/determining-expectancy-in-your-trading/
  winrate <- winrate(x)
  winloss <- winloss(x, extreme=F)
  lossratio <- 1 - winrate
  #   winrate*mean(x[x>0]) + (1-winrate)*mean(x[x<0])
  winrate * winloss - lossratio
}

profitfactor <- function(x, extreme=F) {
  winrate <- winrate(x)
  winloss <- winloss(x, extreme=extreme)
  winloss * winrate / (1 - winrate)
}
