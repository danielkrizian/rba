library(R6); library("data.table"); library(xts) # TODO: remove library headers
Historical <- R6Class("Historical",
                      
                      public = list(data = xts(),
                                    freq=NA,
  initialize = function(data) {
    self$data = data
    return(self)
  },
  
  monthly = function() {
    # find last day of a current month; e.g. turns 2013-11-29 (last BD) to 2013-11-30
    x = self$data
    self$data = to.period(x, period="months", indexAt='yearmon',
                              name=NULL, OHLC=FALSE)
    self$freq = 12L
    return(self)
  }
                      )
)

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