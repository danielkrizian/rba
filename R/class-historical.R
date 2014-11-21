library(R6); library("data.table"); library(xts) # TODO: remove library headers
Historical <- R6Class("Historical",                      
                      public = list(data = NA,
                                    freq = NA,
                                    last = NA,
  initialize = function(data, align = TRUE) {
    data = as.historical(data)
    self$data = data
    self$last = index(data[dim(data)[1]])
    return(self)
  },
  
  measure = function(FUN, ...){
    summary.xts(self$data, FUN, ...)
  },
  
  print = function(){
    print(head(self$data))
    print(tail(self$data))
  }
                      )
)

as.historical = function(x, align, vars){
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
    return(xts(x = x[, val.col, with=FALSE],  order.by=order.by))
}

as.historical.xts = function(x) x

#   compress = function(rule, how="last") {
# #       # grepl(" month | months | month| months|month |months |month|months",rule, ignore.case=TRUE)
# #       # grepl(" hour | hours | hour| hours|hour |hours |hour|hours",rule, ignore.case=TRUE)
# #       # grepl(" min | minutes | min| minutes|min |minutes |min|minutes",rule, ignore.case=TRUE)
# #       # grepl(" sec| secs| seconds|sec|secs|seconds",rule, ignore.case=TRUE)
# #       require(xts)
# #       require(lubridate)
# #       if(grepl(" quarter | quarters | quarter| quarters|quarter |quarters |quarter|quarters"
# #                ,rule, ignore.case=TRUE)) {
# #         if(how=="last")
# #           data <<- data[,.SD[endpoints(eval(as.name(private$time)), on = "quarters")], by=private$id]
# #         freq <<- 4L
# #       }
# #       if(grepl(" month | months | month| months|month |months |month|months"
# #                ,rule, ignore.case=TRUE)) {
# #         if(how=="last") {
# #           data <<- data[,.SD[endpoints(eval(as.name(private$time)), on = "months")], by=private$id]
# #           # find last day of a current month
# #           data[,c(private$time):=floor_date(eval(as.name(private$time)),"month")+months(1)-days(1)] # e.g. 2013-11-29 (last BD) to 2013-11-30
# #           freq <<- 12L
# #         }
# #       }
# #       setkeyv(data, c(private$id, private$time))
# #       return(.self)
#   },