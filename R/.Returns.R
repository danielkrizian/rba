library(lubridate)
make_returns <- function(x, na.pad = T) {
  pch = diff.default(x)/x[2:length(x)]
  if(na.pad)
    c(NA, pch)
  return(pch)
}

returns <- function(x, benchmarks=NULL) {
  ann = periodicity(as.xts(x))$frequency/60/60/24*12
  structure(x, class=c("returns", "xts", "zoo"), ann=ann, benchmarks=benchmarks)
}

as.returns <- function(x, ...) UseMethod("as.returns")

as.returns.data.frame <- function(x, ...) {
  x = .as.xts.data.table(x)
  returns(x, ...)
}

#' Construct returns object
#'
#' Always coerced to wide data format
#' @rdname returns
#' @export returns
as.returns.prices <- function(x, na.pad = TRUE, ...) {
  r = diff.xts(x, na.pad=na.pad)/tail(x, -1)
  r = returns(r, ...)
  xtsAttributes(r) = xtsAttributes(x, user=T)
  r
}

summary.returns <- function(x, weights=NULL, trailing=NULL) {

  benchmarks = xtsAttributes(x)$benchmarks
  assets = setdiff(colnames(x), benchmarks)
  if(!is.null(benchmarks))
    x = x[,c(assets, benchmarks)] # put benchmarks to the end

  l = index(x)[length(index(x))]
  pr = as.prices(x)

  mtd = paste(as.Date(cut(l,"month")) - 1, "::")
  qtd = paste(as.Date(cut(l,"quarter")) - 1, "::")
  ytd = paste(as.Date(cut(l,"year")) - 1, "::")
  l12m = paste(as.Date(l) - months(12), "::")
  performance = rbind("MTD"=as.numeric(pr[NROW(pr)])/as.numeric(pr[mtd][1]) - 1,
                   "QTD"=as.numeric(pr[NROW(pr)])/as.numeric(pr[qtd][1]) - 1,
                   "YTD"=as.numeric(pr[NROW(pr)])/as.numeric(pr[ytd][1]) - 1,
                   "Last 12M"=as.numeric(pr[NROW(pr)])/as.numeric(pr[l12m][1]) - 1,
                   "Vol (ann.)"=summary.xts(x, stdev, ann=xtsAttributes(x)$ann))
  out = list(performance=performance)

  if(!is.null(benchmarks)){
    correlations = NULL
    for(b in benchmarks){
      corr = t(cor(x, x[, b], use="pairwise.complete.obs"))
      corr[,b] = NA
      rownames(corr) = paste("Correl w", b)
      correlations = rbind(correlations, corr)
    }
    out = c(out, list(correlations=correlations))
  }

  if(!is.null(benchmarks) & length(assets)==1){

    capm = capm(x)
    out = c(out, list(capm=capm))
  }
  #     if(!is.null(trailing))
  out

}

# portfolio(r, weights=c(VTI=0.5, ))

capm <- function(x, ...) UseMethod("capm")

capm.default <- function(x, benchmark, Rf=0) {
  R = na.omit(x) - Rf
  bpos = pmatch(benchmark, colnames(R))
  model.lm = lm(R[, -bpos] ~ R[, bpos], R)
  alpha = coef(model.lm)[[1]]
  beta = coef(model.lm)[[2]]
  list(alpha=alpha, beta=beta)
}

capm.returns <- function(x, Rf=0) {

  R = na.omit(x) - Rf
  benchmarks = xtsAttributes(x)$benchmarks
  asset = setdiff(colnames(x), benchmarks)[1]

  out = sapply(benchmarks, function(benchmark){
    bpos = pmatch(benchmark, colnames(R))
    apos = pmatch(asset, colnames(R))
    model.lm = lm(R[, apos] ~ R[, bpos], R)
    alpha = coef(model.lm)[[1]]
    beta = coef(model.lm)[[2]]
    list(alpha=alpha, beta=beta)
  })

  list(alpha=unlist(out["alpha",]), beta=unlist(out["beta",]))
}


#   calcAlpha = function(annualize=T) {
#     # TODO(dk): finalize Returns.alpha. Signature: benchmark data.table, Rf data.table
#     Rf=0
#     data[, list(Alpha=alpha(Return, Benchmark, Rf)), by=Instrument]
#   },
#
#   calendar = function(what=c("MTD", "YTD", "3M", "6M", "years")){
#     freq = 12L
#     warning("Freq fixed at 12 in Returns.calendar. TODO")
#     years <- data[, list(Return=prod(1+Return)-1), by=list(Instrument, Year=year(Date))]
#     years <- dcast.data.table(years, Instrument ~ Year)
#     all <- data[, list("Total (ann.)"=prod(1+Return)^(freq/.N)-1),keyby="Instrument"]
#     out <- merge(all, years)
#     return(out)
#   },
#
#   cov = function(benchmark=FALSE, use='complete.obs', method='pearson'){
#     cov_mat = stats::cov(self$widedata, use=use, method=method)
#     return(cov_mat)
#   },
#
#   cor = function(benchmark=FALSE, use='complete.obs', method='pearson'){
#     if(benchmark){
#       # data[, list(Correlation=cor(Return,Benchmark)),keyby=Instrument]
#       stop("Implement cor benchmark option")
#     } else {
#       cor_mat = stats::cor(self$widedata, use=use, method=method)
#     }
#     return(cor_mat)
#   },
#
#   mean = function() {
#     colMeans(self$widedata)
#   },
#
#   plot = function(drawdowns=T) {
#     datacols=c(.id, .time, .self$index)
#     if(drawdowns)
#       datacols = c(datacols, .self$drawdowns)
#     x = data[, datacols, with=FALSE]
#     x = data.table:::melt.data.table(x, id.vars=c("Instrument","Date"))
#
#     p <- ggplot(x, aes(x=Date, y=value, colour=Instrument)) + geom_line() +
#       #   scale_y_continuous(trans=log10_trans())
#       # p +
#       facet_grid(variable ~ ., scales="free_y") +
#       scale_x_date(breaks=pretty_breaks(n=10), minor_breaks="year", labels=date_format("%Y")) + # scale_x_date(breaks=number_ticks(7), labels=date_format("%Y"))
#       scale_y_continuous(labels = percent_format()) +
#       coord_trans(y="log1p") +
#       #
#       theme_economist_white(gray_bg=FALSE) +
#       scale_colour_economist() +
#       xlab("") + ylab("Cumulative Performance (Log scale)")
#
#     g = ggplotGrob(p)
#     panels = which(sapply(g[["heights"]], "attr", "unit") == "null")
#     g[["heights"]][panels] = list(unit(12, "cm"), unit(3, "cm"))
#     dev.off()
#     grid.draw(g)
#   },
#
#   summary = function(weights=NULL) {
#
#     ann=252
#     compound=T
#     Return = as.name(.col)
#
#     by= if(is.null(weights)) "Instrument" else NULL
#
#     data[, Equity:= cumprod(1+eval(Return)), by=Instrument]
#     data[, Drawdown:= Equity / cummax(Equity) - 1, by=Instrument]
#     dd <- data[, summary.drawdowns(Drawdown, Date), by=Instrument]
#
#     return(data[,list(
#       "CAGR"=annualized(eval(Return), ann=ann, compound=compound)
#       , "Total Return"=cumulative(eval(Return), compound=compound)
#       , "Sharpe"=sharpe(eval(Return), Rf=0, ann=ann)
#       , "Volatility"=sigma(eval(Return), ann=ann)
#       , "R2"=r2(cumprod(1+eval(Return)))
#       , "DVR"= dvr(eval(Return), Rf=0, ann=ann) # dvr(Return)
#       , "MAR" = mar(eval(Return), ann=ann)
#       , "Max Drawdown"= maxdd(eval(Return))
#       , "Average Drawdown"= mean(dd$Depth) #avgdd(Return)
#       , "Average Drawdown Length" = mean(dd$Length)
#     )
#     , by=by])
#   })
