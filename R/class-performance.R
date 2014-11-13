library(R6); library(xts) 
Performance <- R6Class("Performance",
                      public = list(ohlc = NA,
                                    prices = NA,
                                    returns = NA,
                                    benchmarks = NA,
                                    
  summary = function()
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