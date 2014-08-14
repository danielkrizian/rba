library(R6); library("data.table"); library(xts) # TODO: remove library headers
TimeSeries <- R6Class("TimeSeries",
                      private = list(id=NA,
                                     time=NA,
                                     measure=NA,
                                     wide=FALSE,
  coerce_wide = function() {
    if(!private$wide) {
      use.cols = c(private$id, private$time, private$measure)
      self$widedata = 
        dcast.data.table(self$talldata[, use.cols, with=FALSE], 
                         formula = as.formula(paste0(private$time, "~", 
                                                     private$id )))
      idx = self$widedata[[private$time]] # TODO: accommodate IDate and yearmon
      self$widedata = xts(self$widedata[, c(private$time) := NULL], 
                          order.by=idx)
      private$wide = TRUE
      warning("Tall dataset converted to wide format.")
    }
  }),

                      public = list(talldata = data.table(),
                                    widedata = xts(),
                                    select=NA,
                                    period=NA,
                                    freq=NA,
  initialize = function(..., 
                        data=data.table(),
                        widedata=xts(),
                        talldata=data.table(),
                        val.col=character(),
                        id.col=character(),
                        time.col=character(),
                        tag=NULL) {
    if(!missing(widedata)) {
      self$widedata = widedata
      private$wide = TRUE
    } else {
      if(missing(val.col) | missing(id.col) | missing(time.col)) {
        cols = detect_cols(data)
        id.col = cols$id.col[1]
        time.col = cols$time.col[1]
        val.col = if(missing(val.col)) cols$val.col[1] else val.col
      }
      
      if(is.null(id.col)) {
        self$widedata = data
        private$wide = TRUE
      } else {
        private$id = id.col
        private$time = time.col
        private$measure = val.col
        private$wide = FALSE
      }
      if(missing(talldata))
        self$talldata = data
      else
        self$talldata = talldata
    }
    self$tag = tag
    return(self)
  },
  
  print = function() {
    if(private$wide){
      print(self$widedata)
    } else {
      print(self$talldata)
    }
  },
  
  monthly = function() {
    # find last day of a current month; e.g. turns 2013-11-29 (last BD) to 2013-11-30
    private$coerce_wide()
    self$widedata = to.period(self$widedata, period="months", indexAt='yearmon',
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