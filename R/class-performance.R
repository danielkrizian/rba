library(R6); library(xts) 
Performance <- R6Class("Performance",
                       inherit = Historical,
                       private = list(ohlc=NA, prices=NA, returns=NA),
                       active = list(
  ohlc = function(value) {"TODO"},
  prices = function(value) {
    if(missing(value)) private$prices 
    else {
      private$prices = value
      private$returns = value$returns()
    }
  },
  returns = function(value){}
                         
                         ),
  
                       public = list(benchmarks = NA,
  initialize = function(benchmarks = NULL) {
    if(!is.null(benchmarks))
      self$benchmarks = benchmarks  
    return(self)
  },
  
  cum =  function(subset=NULL){
    p = self$prices$data
    subperiod = if(missing(subset)) coredata(p) else coredata(p[subset])
    performance_result = subperiod[NROW(subperiod), ]/subperiod[1, ] - 1
  },

  summary = function(na.rm=T, annual=T){
    l = self$last
    returns = self$returns
    browser()
    rbind("MTD"=self$cum(subset = paste(as.Date(cut(l,"month")) - 1, "::")),
          "QTD"=self$cum(subset = paste(as.Date(cut(l,"quarter")) - 1, "::")),
          "YTD"=self$cum(subset = paste(as.Date(cut(l,"year")) - 1, "::")),
          "Last 12M"=self$cum(subset = paste(as.Date(l) - months(12), "::")),
          "Vol (ann.)"=returns$measure( stdev, ann=returns$freq))
  }
                       )
)

performance = function(x){
  UseMethod("performance")
}

performance.Prices = function(x, benchmarks=NULL) {
  p = Performance$new(benchmarks=benchmarks)
  p$prices = x
  return(p)
}

performance.Returns = function(x, benchmarks=NULL) {
  p = Performance$new(benchmarks=benchmarks)
  p$returns = x
  p$prices = x$perf_index()
  p$last = x$last
  return(p)
}

performance.OHLC = function(x) {
  # TODO: complete performance.OHLC
  p = Performance$new()
  p$ohlc = x
  p$last = x$last
  return(p)
}

