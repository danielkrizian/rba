library(R6); library(xts) 
Performance <- R6Class("Performance",
                       inherit = Historical,
                       public = list(ohlc = NA,
                                     prices = NA,
                                     returns = NA,
                                     benchmarks = NA,
  initialize = function(benchmarks = NULL) {
    if(!is.null(benchmarks))
      self$benchmarks = benchmarks  
    return(self)
  },
  
  forperiod =  function(subset){
    p = self$prices
    subperiod = coredata(p[subset])
    performance_result = subperiod[NROW(subperiod), ]/subperiod[1, ] - 1
  }
  
  mtd = function(){
    prev.eom = self$last - days(day(self$last))
    self$forperiod( paste(prev.eom, sep="::") )
  },
  
  qtd = function(){
    last = self$last
    last = as.POSIXct("2014-01-03")
    prev.eoq = 
      c(1, 4, 7, 10)
    year(last)
    quarter
    + quarter
      3*(quarter(last)-1)
    self$forperiod( paste(prev.eoq, sep="::") )
  }
  
  ytd = function(){
    prev.eoy = self$last - days(yday(self$last))
    self$forperiod( paste(prev.eoy, sep="::") )
  },
  
  l12m = function(){
    ago.12m = self$last - months(12)
    self$forperiod( paste(ago.12m, sep="::") )
  },
        
  summary = function(){
    p = self$prices$data
    l = self$last
    

    
    browser()
  }
                       )
)

performance = function(x){
  UseMethod("performance")
}

performance.Prices = function(x, benchmarks=NULL) {
  p = Performance$new(benchmarks=benchmarks)
  p$prices = x
  p$returns = x$returns()
  p$last = x$last
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

