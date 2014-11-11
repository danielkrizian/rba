library(quantmod); library(xts) # # TODO: remove quantmod dependency

#' Prices is a class for representing price data
#' 
#' 
Prices <- R6Class("Prices",
                  lock=FALSE,
                  inherit = Historical,
                  public = list(
  
  returns = function() {
    x = self$data
    data = xts::diff.xts(x, na.pad=FALSE)
    r = Returns$new(data=data)
    return(r)
  }
                  )
)

OHLC <- R6Class("OHLC",
                lock=FALSE,
                inherit = Prices,
                public = list(
                  data = list(),
                  op.col = NULL,
                  cl.col = NULL,
                  hi.col = NULL,
                  lo.col = NULL,
  initialize = function(...) {
    super$initialize(...)
  },
  
#   print = function() {
#     message("implement print method for OHLC")
#   },
  
  monthly = function() {

    x = self$data
    self$data = to.period(x, indexAt='yearmon', name=NULL, OHLC=TRUE)
    self$freq = 12L
    return(self)
  },
  
  ClCl = function() {
    x = self$data
    data = quantmod::ClCl(x)
      r = Returns$new(data)
      return(r)
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
               lo.col=colnames(x)[grep("(?<!c)lo", colnames(x), ignore.case=T, 
                                       perl = T)])
  return(o)
}

prices <- function(x, val.col, ...) {
  p = Prices$new(data=x, val.col=val.col, ...)
  return(p)
}
                                     