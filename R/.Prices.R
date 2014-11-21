library(quantmod); library(xts) # # TODO: remove quantmod dependency

#' Prices is a class for representing price data
#' 
#' 
Prices <- R6Class("Prices",
                  lock=FALSE,
                  inherit = Historical,
                  private = list(returns_object = NA),
                  active = list(
                    
  data = function(value) {
    if(missing(value)) 
      private$data
    else 
      private$data = value
  },
  
  returns = function(value) {
    if(missing(value)) 
      private$returns_object$data
    else 
      private$returns_object$data = value 
    # TODO through Reference Class or [ update
    # make sure they are same frequency as prices
  }
                      ),
  
                  public = list(
                    
  initialize = function(data, track.returns=TRUE){
    self$data = data
    if(track.returns) {
      browser()
      self$returns = pch(data, na.pad=TRUE)
    }
    return(self)
  },
  
  monthly = function() {
    # find last day of a current month; e.g. turns 2013-11-29 (last BD) to 2013-11-30
    self$data = to.period(x, period="months", indexAt='endof',
                          name=NULL, OHLC=FALSE)
    self$freq = 12L
    return(self)
  },
  
  print = function (){
    print("Prices")
    super$print()
  }
                  )
)

prices <- function(x, track.returns=TRUE) {
  p = Prices$new(data=x, track.returns=track.returns)
  return(p)
}

as.prices <- function(x) UseMethod("as.prices")

as.prices.data.frame <- function(x) {
  Prices$new(x)
}
                                     