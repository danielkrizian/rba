
.select.assets <- function(x, weights) {
  benchmarks = xtsAttributes(x)$benchmarks
  assets = setdiff(names(x), benchmarks)
  select.assets = intersect(names(weights), assets)
  if(!length(select.assets))
    stop("Weight names do not match any of the assets.")
  return(select.assets)
}

#' Construct portfolio from returns
#'
#' portfolio(r, weights=c(VTI=0.5, ))
portfolio <- function(x, ...) {
  UseMethod("portfolio", x)
}

portfolio.returns <- function(x, weights, name="Portfolio") {
  # TODO: integrate with strategery Portfolio class
  # TODO: see portfolio.R to integrate rebalancing schemes
  benchmarks = xtsAttributes(x)$benchmarks
  Ra = x[,names(weights)] #x[,-pmatch(benchmarks, colnames(x))]
  if(na.omit)
    Ra = na.omit(Ra)
  else {} # na.omit=FALSE reallocate weights from missing instruments

  if(length(names(weights))) # ensure alignment of weights with returns
    weights = weights[colnames(Ra)]

  out = PerformanceAnalytics::Return.portfolio(Ra, weights = weights,
                                               wealth.index = FALSE,
                                               contribution = FALSE,
                                               geometric = TRUE)
  colnames(out) = c(name)
  out = returns(merge(out, x[, benchmarks]), benchmarks = benchmarks)
  out
}



# calculate portfolio returns with rebalancing, allowing for leading NAs

portfolio.returns <- function (x, weights, rebalance=T, na.alloc=F, label="Portfolio") {

  assets = x[, .select.assets(x, weights)]
  if(rebalance & is.vector(weights) & !na.alloc) {
    assets[,1] = rowSums(assets * weights, na.rm = F)
    pfolio = assets[, 1]
    names(pfolio) = label
    pfolio = returns(merge(pfolio, x[, xtsAttributes(x)$benchmarks]))
    return(pfolio)
  }
  else stop("Implement custom rebalance")
}

portfolio.prices <- function (x, weights, rebalance=T, na.alloc=F) {

  stop("Portfolio from Prices and custom/subset rebalancing not implemented yet.")
  # weights <- na.fill(weights, fill=0)

  # reb <- PDT[,.SD[c(1,endpoints(Date, on="years")),], by=Name][,list(Name,Date)]
  # setkey(reb,Name,Date)
  # rebalance <- copy(PDT)
  # rebalance[,Value:=NA]
  # rebalance[reb,Value:=1]
  # rebalance <- as.xts(dcast.data.table(rebalance, Date~Name))

  # weights, shares, positions

  illiquid <- apply(assets, 2
                    , function(p) ifelse(is.na(p)
                                         , na.fill(p, fill=c(FALSE, TRUE, TRUE))
                                         ,FALSE))

  in.universe <- !is.na(assets) | illiquid # loses xts class
  browser()
  w.portf <- in.universe * weights
  w.subset <- w.portf/rowSums(w.portf)

  leverage <- rowSums(P.val, na.rm=T) / P.val
  # if(length(rebalance)) {
  leverage[] <- ifelse(is.na(rebalance),NA,leverage)
  leverage <- na.locf(leverage, na.rm=F)
  # }

  shares <- w.subset * leverage
  positions <- shares * P.val
  P.ror <- diff.xts(P.val, arithmetic=FALSE) - 1
  pnl <- rowSums(lag.xts(positions) * P.ror, na.rm=T) # daily P&L

  GMV <- rowSums(abs(positions), na.rm=T) # gross market value of the portfolio

  R.portf <- xts(pnl / lag.xts(GMV), order.by=index(P))
  R.portf <- na.fill(R.portf,fill=0)
  P.portf <- cumprod(R.portf + 1)
  R.portf <- diff(to.interval(P.portf, to="months"),arithmetic=FALSE) - 1
}
