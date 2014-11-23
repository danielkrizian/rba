library(R6); library("data.table"); library(xts) # TODO: remove library headers
Historical <- R6Class("Historical",
                      private = list(.data = NA),
                      active = list(
  data = function(value) {
    if(missing(value))
      private$.data
    else
      private$.data = value
  }),

                      public = list(freq = NA,
                                    last = NA,

  initialize = function(data) {
    data = as.historical(data)
    self$data = data
    self$last = index(data[dim(data)[1]])
    return(self)
  },

  compress = function(period, how){
    csetup = .compression(period)
    period = csetup$xtsstyle
    self$data = to.period(self$data, period, indexAt='endof', name=NULL, OHLC=FALSE)
    self$freq = csetup$freq
    return(self)
  },

  measure = function(FUN, ...){
    summary.xts(private$.data, FUN, ...)
  },

  print = function(){
    print(head(private$.data))
    print(tail(private$.data))
  }
                      )
)

as.historical = function(x){
  UseMethod("as.historical")
}

as.historical.data.table = function(x){
  cols = detect_cols(x)
  id.col = cols$id.col[1]
  time.col = cols$time.col[1]
  val.col = cols$val.col
  use.cols = c(id.col, time.col, val.col)
    if(!is.null(id.col)) {
      x = dcast.data.table(x[, use.cols, with=FALSE],
                           formula = as.formula(paste0(time.col, "~",
                                                       id.col )))
      message("Tall dataset converted to wide format.")
    }
  order.by = as.POSIXct(x[[time.col]])
  out = xts(x = x[, val.col, with=FALSE],  order.by=order.by)
  return(out)
}

as.historical.xts = function(x) {
  x
}

.compression = function(x) {
      # grepl(" month | months | month| months|month |months |month|months",rule, ignore.case=TRUE)
      # grepl(" hour | hours | hour| hours|hour |hours |hour|hours",rule, ignore.case=TRUE)
      # grepl(" min | minutes | min| minutes|min |minutes |min|minutes",rule, ignore.case=TRUE)
      # grepl(" sec| secs| seconds|sec|secs|seconds",rule, ignore.case=TRUE)

      if(grepl(" quarter | quarters | quarter| quarters|quarter |quarters |quarter|quarters"
               ,x, ignore.case=TRUE)) {
        xtsstyle="quarters"
        freq = 4L
      }
      if(grepl(" month | months | month| months|month |months |month|months"
               ,x, ignore.case=TRUE)) {
        xtsstyle="months"
        freq = 12L
      }
      return(list(xtsstyle=xtsstyle, freq=freq))
}
