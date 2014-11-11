RoR <- pchange <- function(x, na.pad=T) {
  RoR = diff.default(x)/x[-length(x)]
  if(na.pad)
    c(NA, RoR)
  else
    RoR
}

change <- function(x, na.pad=T) {
  if(na.pad)
    c(NA, diff.default(x))
  else
    diff.default(x)
}


#' Construct returns object
#'
#' Always coerced to wide data format
#' @rdname returns
#' @export returns
returns <- function(x, val.col=NULL, benchmark=NULL, ...) {
  r = Returns$new(data=x, val.col = val.col, benchmark = benchmark, ...)
  return(r)
}

Returns <- R6Class('Returns',
                   lock = FALSE,
                   inherit = Historical,
                   public = list(
                     benchmarks = NA,
                                 
  initialize = function(...) {
    super$initialize(...)
  },
  
  calcAlpha = function(annualize=T) {
    # TODO(dk): finalize Returns.alpha. Signature: benchmark data.table, Rf data.table
    Rf=0
    data[, list(Alpha=alpha(Return, Benchmark, Rf)), by=Instrument]
  },

  calendar = function(what=c("MTD", "YTD", "3M", "6M", "years")){
    freq = 12L
    warning("Freq fixed at 12 in Returns.calendar. TODO")
    years <- data[, list(Return=prod(1+Return)-1), by=list(Instrument, Year=year(Date))]
    years <- dcast.data.table(years, Instrument ~ Year)
    all <- data[, list("Total (ann.)"=prod(1+Return)^(freq/.N)-1),keyby="Instrument"]
    out <- merge(all, years)
    return(out)
  },
  
  cov = function(benchmark=FALSE, use='complete.obs', method='pearson'){
      cov_mat = stats::cov(self$widedata, use=use, method=method)
    return(cov_mat)
  },

  cor = function(benchmark=FALSE, use='complete.obs', method='pearson'){
    if(benchmark){
      # data[, list(Correlation=cor(Return,Benchmark)),keyby=Instrument]
      stop("Implement cor benchmark option")
    } else {
      cor_mat = stats::cor(self$widedata, use=use, method=method)
    }
    return(cor_mat)
  },
  
  mean = function() {
    colMeans(self$widedata)
  },

  plot = function(drawdowns=T) {
    datacols=c(.id, .time, .self$index)
    if(drawdowns)
      datacols = c(datacols, .self$drawdowns)
    x = data[, datacols, with=FALSE]
    x = data.table:::melt.data.table(x, id.vars=c("Instrument","Date"))

    p <- ggplot(x, aes(x=Date, y=value, colour=Instrument)) + geom_line() +
      #   scale_y_continuous(trans=log10_trans())
      # p +
      facet_grid(variable ~ ., scales="free_y") +
      scale_x_date(breaks=pretty_breaks(n=10), minor_breaks="year", labels=date_format("%Y")) + # scale_x_date(breaks=number_ticks(7), labels=date_format("%Y"))
      scale_y_continuous(labels = percent_format()) +
      coord_trans(y="log1p") +
      #
      theme_economist_white(gray_bg=FALSE) +
      scale_colour_economist() +
      xlab("") + ylab("Cumulative Performance (Log scale)")

    g = ggplotGrob(p)
    panels = which(sapply(g[["heights"]], "attr", "unit") == "null")
    g[["heights"]][panels] = list(unit(12, "cm"), unit(3, "cm"))
    dev.off()
    grid.draw(g)
  },

  summary = function(weights=NULL) {

    ann=252
    compound=T
    Return = as.name(.col)

    by= if(is.null(weights)) "Instrument" else NULL

    data[, Equity:= cumprod(1+eval(Return)), by=Instrument]
    data[, Drawdown:= Equity / cummax(Equity) - 1, by=Instrument]
    dd <- data[, summary.drawdowns(Drawdown, Date), by=Instrument]

    return(data[,list(
      "CAGR"=annualized(eval(Return), ann=ann, compound=compound)
      , "Total Return"=cumulative(eval(Return), compound=compound)
      , "Sharpe"=sharpe(eval(Return), Rf=0, ann=ann)
      , "Volatility"=sigma(eval(Return), ann=ann)
      , "R2"=r2(cumprod(1+eval(Return)))
      , "DVR"= dvr(eval(Return), Rf=0, ann=ann) # dvr(Return)
      , "MAR" = mar(eval(Return), ann=ann)
      , "Max Drawdown"= maxdd(eval(Return))
      , "Average Drawdown"= mean(dd$Depth) #avgdd(Return)
      , "Average Drawdown Length" = mean(dd$Length)
    )
    , by=by])
  })
)

