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
                  
                  monthly = function() {
                    
                    x = self$data
                    self$data = to.period(x, indexAt='endof', name=NULL, OHLC=TRUE)
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