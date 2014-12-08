library(lubridate)
make_returns <- function(x, na.pad = T) {
  pch = diff.default(x)/x[2:length(x)]
  if(na.pad)
    c(NA, pch)
  return(pch)
}

#' Detect returns data
#'
#' Returns TRUE/FALSE, with AIC of the distribution fit as an attribute
#' Stub only, TODO: finalize
like.returns <- function(x){
  if(!is.numeric(x))
    return(FALSE)

  library(MASS)
  normal = fitdistr(na.omit(x), "normal")
  #   fitdistrplus::descdist(x, graph=F)

  stats::AIC(normal) < 300
  # fit = normal$estimate
  # as.logical(abs(fit["mean"]) < 1 & abs(fit["sd"])<5* abs(fit["mean"]))
}

# TODO: implement checking
returns <- function(x, benchmarks=NULL, na.warn=FALSE) {

  scale = periodicity(as.xts(x))$scale
  ann = switch(scale,
               "yearly"=1,
               "quarterly"=4,
               "monthly"=12,
               "weekly"=52,
               "daily"=252,
               "hourly"=252*8) # TODO: ann value for hourly data

  # fill in non-leading NAs with zero
  x = xtsrunapply(x, function(col) na.fill(col, list(NA, 0, 0)))
  # alternative: in xts:::naCheck, use
  # .Call("naCheck", x, FALSE, PACKAGE = "xts")
  # na.fill(nonleading, fill=0)

  obj = structure(x, class=c("returns", "xts", "zoo"), ann=ann)
  obj = set_benchmarks(obj, benchmarks)
  return(obj)
}

as.returns <- function(x, ...) UseMethod("as.returns")

as.returns.returns <- function(x, benchmarks=NULL) {
  set_benchmarks(x, benchmarks)
}

as.returns.data.frame <- function(x, ...) {
  lr = unlist(lapply(x, function(col) like.returns(col)))
  x = .as.xts.data.table(x, val.col=names(lr[lr]) )
  returns(x, ...)
}

#' Construct returns object
#'
#' Always coerced to wide data format
#' @rdname returns
#' @export returns
as.returns.prices <- function(x, na.pad = TRUE, ...) {
  r = diff.xts(x, na.pad=na.pad)/lag.xts(x, na.pad=T)
  colnames(r) = colnames(x) # xts bug, see http://stackoverflow.com/questions/14420332/is-there-a-way-to-get-merge-xts-to-leave-names-unchanged
  r = returns(r, ...)
  xtsAttributes(r) = xtsAttributes(x, user=T)
  r
}

# merge.returns <- function(...) {
#   dots = list(...)
#   # TODO: handle duplicates
#   browser()
#   benchmarks = sapply(dots, function(x) xtsAttributes(x)$benchmarks)
#   out = merge.xts(...)
#   names(out) = merged.names # merge.xts does check.names
# }

print.returns <- function(x) {
  print(round(x*100, 2))
}

# print.trailing <- function(x) {
#   print(round(x*100, 2))
# }

summary.returns <- function(x, weights=NULL, trailing=NULL) {

  benchmarks = xtsAttributes(x)$benchmarks

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
                   "Vol (ann.)"=calculate(x, stdev, ann=xtsAttributes(x)$ann))
  out = list(performance=performance)

  if(!is.null(benchmarks) & length(assets)==1){

    capm = capm(x)
    out = c(out, list(capm=capm))
  }
  #     if(!is.null(trailing))
  out

}

cor <- function(x, ...) UseMethod("cor")

cor.default <- function(x, y = NULL, use = "everything",
                               method = c("pearson", "kendall", "spearman")){
  stats::cor(x, y, use, method)
}

#' @param all display all cross-correlations. FALSE for asset vs. benchmark only
cor.returns <- function(x, all=FALSE, use = "pairwise.complete.obs", method="pearson") {
  out = stats::cor(x, use=use, method=method)
  if(!all) {
    benchmarks = xtsAttributes(x)$benchmarks
    if(!is.null(benchmarks)){
      for(b in match(benchmarks, colnames(x)))
        out[b, b] = NA

      out = out[benchmarks, ]
    }
  }
    out
}


#### PERFORMANCE RELATIVE (CAPM) ####

capm <- function(x, ...) UseMethod("capm")

capm.returns <- function(x, Rf=0) {
  R = na.omit(x) - Rf
  benchmarks = xtsAttributes(x)$benchmarks
  assets = setdiff(colnames(x), benchmarks)

  alpha = NULL
  beta = NULL
  lapply(benchmarks, function(benchmark){
    bpos = pmatch(benchmark, colnames(R))
    apos = pmatch(assets, colnames(R))
    model.lm = lm(R[, apos] ~ R[, bpos], R)
    add.alpha = if(length(apos)>1) coef(model.lm)[1,] else coef(model.lm)[[1]]
    add.beta  = if(length(apos)>1) coef(model.lm)[2,] else coef(model.lm)[[2]]
    add.alpha = t(add.alpha); rownames(add.alpha) = benchmark
    add.beta = t(add.beta); rownames(add.beta) = benchmark
    alpha <<- rbind(alpha, add.alpha)
    beta <<- rbind(beta, add.beta)
    invisible(list())
  })

  list(Alpha=alpha, Beta=beta)
}

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
