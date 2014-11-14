library(R6); library(xts) 
Performance <- R6Class("Performance",
                       public = list(ohlc = NA,
                                     prices = NA,
                                     returns = NA,
                                     benchmarks = NA,
  initialize = function(..., benchmarks = NULL) {
    if(!is.null(benchmarks))
      self$benchmarks = benchmarks  
    return(self)
  },
  
  wtd = function(){
  },
        
  summary = function(){}
                      )
)

performance = function(x){
  UseMethod("performance")
}

performance.Prices = function(x) {
  p = Performance$new()
  p$prices = x
  return(p)
}

performance.OHLC = function(x) {
  p = Performance$new()
  p$ohlc = x
  return(p)
}

performance.Returns = function(x) {
  p = Performance$new()
  p$returns = x
  return(p)
}