TimeSeries <- R6Class("TimeSeries",
                      private = list(.id=NA,
                                     .time=NA,
                                     .value=NA,
                                     .freq=NA),
                      public = list(data = data.table(),
                                    select=NA,
                                    period=NA,
  initialize = function(..., 
                        data=data.table(),
                        val.col=character(),
                        id.col=character(),
                        time.col=character(),
                        tag=NULL) {
    
    if(!length(data))
      return(self)
    
    cols = detect_cols(data)
    id.col = cols$id.col[1]
    time.col = cols$time.col[1]
    val.col = if(missing(val.col)) cols$val.col[1] else val.col
    
    self$data <- data
    self$tag <<- tag
    private$.id <- id.col
    private$.time <- time.col
    private$.value <- val.col
    return(self)
  },
  
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
  # #           data <<- data[,.SD[endpoints(eval(as.name(.time)), on = "quarters")], by=.id]
  # #         freq <<- 4L
  # #       }
  # #       if(grepl(" month | months | month| months|month |months |month|months"
  # #                ,rule, ignore.case=TRUE)) {
  # #         if(how=="last") {
  # #           data <<- data[,.SD[endpoints(eval(as.name(.time)), on = "months")], by=.id]
  # #           # find last day of a current month
  # #           data[,c(.time):=floor_date(eval(as.name(.time)),"month")+months(1)-days(1)] # e.g. 2013-11-29 (last BD) to 2013-11-30
  # #           freq <<- 12L
  # #         }
  # #       }
  # #       setkeyv(data, c(.id, .time))
  # #       return(.self)
  #   },
  
  monthly = function() {
    require(xts)
    require(lubridate)
    data <<- data[,.SD[endpoints(eval(as.name(.time)), on = "months")], by=.id]
    # find last day of a current month; e.g. turns 2013-11-29 (last BD) to 2013-11-30
    data[,c(.time):=floor_date(eval(as.name(.time)),"month")+months(1)-days(1)] 
    .freq <<- 12L
    return(self)
  }
                      )
)