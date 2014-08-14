library(quantmod) # # TODO: remove quantmod dependency

#' Prices is a class for representing price data
#' 
#' 
Prices <- R6Class("Prices",
                  lock=FALSE,
                  inherit = TimeSeries,
                  public = list(
  initialize= function(..., val.col) {
    super$initialize(..., val.col=val.col)
  },
  
  returns = function() {
    private$coerce_wide()
    data = xts::diff.xts(self$widedata, na.pad=FALSE)
    r = Returns$new(widedata=data)
    return(r)
  }
                  )
)

OHLC <- R6Class("OHLC",
                lock=FALSE,
                inherit = Prices,
                public = list(
                  op.col = NULL,
                  cl.col = NULL,
                  hi.col = NULL,
                  lo.col = NULL,
  initialize = function(...) {
    super$initialize(...)
    args=list(...)
    self$op.col = args$op.col
    self$cl.col = args$cl.col
    self$hi.col = args$hi.col
    self$lo.col = args$lo.col
  },
  
#   print = function() {
#     message("implement print method for OHLC")
#   },
  
  monthly = function() {

    if(private$wide) {
      self$widedata = to.period(self$widedata, indexAt='yearmon', 
                                name=NULL, OHLC=TRUE)
    } else {
      library(xts)
      library(lubridate)
      data = self$talldata
      data = data[, list(Date=as.yearmon(last(eval(as.name(private$time)))), 
                         Open=first(eval(as.name(self$op.col))),
                         High=max(eval(as.name(self$hi.col))),
                         Low=min(eval(as.name(self$lo.col))),
                         Close=last(eval(as.name(self$cl.col)))),
                  by=list(eval(as.name(private$id)), 
                          year(eval(as.name(private$time))), 
                          month(eval(as.name(private$time))))]
      # fixes bug above manually https://github.com/Rdatatable/data.table/issues/750
      setnames(data, "as.name", private$id) 
      data = data[, list(eval(as.name(private$id)), Date, Open, High, Low, Close)]
      setkeyv(data, c(private$id, "Date"))
      self$talldata = data
    }
    self$op.col = "Open"
    self$cl.col = "Close"
    self$hi.col = "High"
    self$lo.col = "Low"
    self$freq = 12L
    return(self)
  },
  
  ClCl = function() {
    if(private$wide){
      self$widedata = quantmod::ClCl(self$widedata)
      colnames(self$widedata) = "Return"
    } else {
      expr = as.call(parse(text="quantmod::ClCl(.SD)"))[[1]]
      self$talldata[,Return:=eval(expr), by=c(private$id)]
      r = Returns$new(talldata=self$talldata, id.col=private$id, 
                      time.col=private$time, val.col="Return")
    # r$private$coerce_wide()
      return(r)
    }
  },
  
  returns = function() {
    return(self$ClCl())
  }
                )
)


#' OHLC is a class for representing OHLCV data
ohlc <- function(x, val.cols=NULL) {
  o = OHLC$new(data=x,
               op.col=colnames(x)[grep("op", colnames(x), ignore.case=T)],
               cl.col=colnames(x)[grep("cl", colnames(x), ignore.case=T)],
               hi.col=colnames(x)[grep("hi", colnames(x), ignore.case=T)],
               lo.col=colnames(x)[grep("(?<!c)lo", 
                                       colnames(x), 
                                       ignore.case=T, 
                                       perl = T)])
  return(o)
}

prices <- function(x, val.col, ...) {
  p = Prices$new(data=x, val.col=val.col, ...)
  return(p)
}
                                     