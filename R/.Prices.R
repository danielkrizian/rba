library(quantmod); library(xts) # # TODO: remove quantmod dependency

#' Prices is a class for representing price data
#'
#'
Prices <- R6Class("Prices",
                  lock=FALSE,
                  inherit = Historical,
                  private = list(returns_object = NA,

  calculate_returns = function(){
    obj = Returns$new(pch(self$data, na.pad=TRUE), track.prices=FALSE)
    obj$prices = self
    self$returns = obj
  }
                                 ),
                  active = list(

  returns = function(value) {
    if(missing(value))
      private$returns_object
    else {
      private$returns_object = value
    }
    # TODO through Reference Class or [ update
    # make sure they are same frequency as prices
  }
                      ),

                  public = list(

  initialize = function(data, track.returns=TRUE){
    super$initialize(data)
    if(track.returns)
      private$calculate_returns()
    return(self)
  },

  compress = function(period){
    super$compress(period)
    private$calculate_returns()
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

as.prices.data.frame <- function(x, track.returns=TRUE) {
  prices(x, track.returns)
}

`[.Prices` <- function(x, i, j, period) {

}
