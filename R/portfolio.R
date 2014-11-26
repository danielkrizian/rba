#' Construct portfolio from returns
#' 
#' portfolio(r, weights=c(VTI=0.5, ))
portfolio <- function(x, ...) UseMethod("portfolio")

portfolio.returns <- function(x, weights, na.omit=TRUE, name="Portfolio") {
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
#
# w <- w[assets] #align/reorder with price frame
# names(w) <- assets
# w <- na.fill(w,fill=0)/100
#
# reb <- PDT[,.SD[c(1,endpoints(Date, on="years")),], by=Name][,list(Name,Date)]
# setkey(reb,Name,Date)
# rebalance <- copy(PDT)
# rebalance[,Value:=NA]
# rebalance[reb,Value:=1]
# rebalance <- as.xts(dcast.data.table(rebalance, Date~Name))
# P <- as.xts(dcast.data.table(PDT, Date~Name))
#
# # weights
# # shares
# # positions
#
# illiquid <- apply(P, 2
#                   , function(p) ifelse(is.na(p)
#                                        , na.fill(p, fill=c(FALSE, TRUE, TRUE))
#                                        ,FALSE))
#
# in.universe <- !is.na(P) | illiquid
#
# w.portf <- t(t(in.universe) * w)
# w.subset <- w.portf/rowSums(w.portf)
#
# # Price used for portfolio holdings in valuation.
# # Can be illiquid/non-market/locf-ed price. Price is locf-ed for inside NAs only
# # (no leading, no trailing)
# P.val <- na.locf( P, na.rm = FALSE)
# leverage <- rowSums(P.val, na.rm=T)/P.val
# # if(length(rebalance)) {
# leverage[] <- ifelse(is.na(rebalance),NA,leverage)
# leverage <- na.locf(leverage, na.rm=F)
# # }
#
# shares <- w.subset * leverage
# positions <- shares * P.val
# P.ror <- diff.xts(P.val, arithmetic=FALSE) - 1
# pnl <- rowSums(lag.xts(positions) * P.ror, na.rm=T) # daily P&L
#
# GMV <- rowSums(abs(positions), na.rm=T) # gross market value of the portfolio
#
# R.portf <- xts(pnl / lag.xts(GMV), order.by=index(P))
# R.portf <- na.fill(R.portf,fill=0)
# P.portf <- cumprod(R.portf + 1)
# R.portf <- diff(to.interval(P.portf, to="months"),arithmetic=FALSE) - 1
