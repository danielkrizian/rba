#' Construct returns object
#'
#' @rdname returns
#' @export returns
returns <- function(data, as.prices=FALSE, use.cols=NULL, benchmark=NULL) {
  
  data = data[, use.cols, with=FALSE]
  cols = detect_cols(data)
    
  if(as.prices) {
    
    # calc
  }
  
  r = Returns$new(data=data,
                  id.col=cols$id.col[1],
                  time.col=cols$time.col[1],
                  val.col=cols$val[1])
  return(r)
}

calc_returns <- function(data) {
  require(quantmod)
  data[,Price:=getPrice(data)]
}

#' returns list of column names representing id, time, value
detect_cols = function(data){

  timeBased = function(x) {
    if (!any(sapply(c("Date", "POSIXt", "chron",
                      "dates", "times",
                      "timeDate", "yearmon",
                      "yearqtr", "xtime"), function(xx) inherits(x, xx)))) {
      FALSE
    } else TRUE
  }
  
  get_rated_col = function(data, score) {
    score = as.numeric(score)
    top = names(data)[score == max(score)]
  }

  inkey = names(data) %in% key(data)
  ischar = vapply(data, is.character, 1, USE.NAMES=F)
  isfac = vapply(data, is.factor, 1, USE.NAMES=F)
  istimebased= vapply(data, timeBased, 1, USE.NAMES=F)
  isnumeric = vapply(data, is.numeric, 1, USE.NAMES=F)
  isinteger = vapply(data, is.integer, 1, USE.NAMES=F)
  isreturn = vapply(names(data), grepl, 1, USE.NAMES=F, pattern="ret", 
                    ignore.case=TRUE)
  
  id.col = get_rated_col(data, inkey + ischar + isfac + 0.5*isinteger)
  time.col = get_rated_col(data, inkey + istimebased)
  val.col = get_rated_col(data, 
                          score= -inkey - istimebased - ischar + 
                            isnumeric + 5*isreturn)

  return(list(id.col=id.col, time.col=time.col, val.col=val.col))
}

Returns = setRefClass('Returns',
                      fields= list(data="data.table",
                                   .id="character",
                                   .time="character",
                                   .value="character",
                                   .freq="character",
                                   select="character",
                                   period="Date",
                                   benchmarks="character"),

                      methods = list(

  initialize = function(..., data=data.table(),
                         val.col=character(),
                         id.col=character(),
                         time.col=character()) {
    callSuper(...)
    
    if(!length(data))
      return(.self)
    .self$data <- data
    .self$.id <- id.col
    .self$.time <- time.col
    .self$.value <- val.col
    return(.self)
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

  correlation = function(with="Benchmark"){
    if(identical(with,"Benchmark"))
      data[, list(Correlation=cor(Return,Benchmark)),keyby=Instrument]
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

