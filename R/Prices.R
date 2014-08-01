
Prices <- R6Class("Prices",
                  lock=FALSE,
                  inherit = TimeSeries,
                  public = list(
  initialize= function(..., val.col) {
    super$initialize(..., val.col=val.col)
  },
  
  returns = function() {
    expr = as.call(parse(text=paste0("pchange(", .value, ")")))[[1]]
    require(quantmod) # TODO: remove quantmod dependency
    data[,Return:=eval(expr), by=.id] #TODO: optimize with .SDcols
    r = Returns$new(data=data,
                    id.col=.id,
                    time.col=.time,
                    val.col="Return")
    return(r)
  }
                  )
)

OHLC <- R6Class("OHLC",
                lock=FALSE,
                inherit = Prices,
                public = list(
                  op.col = NULL,
                  cl.col = NULL,
                  hi.col = NULL,
                  lo.col = NULL,
  initialize = function(...) {
    super$initialize(...)
    args=list(...)
    op.col <<- args$op.col
    cl.col <<- args$cl.col
    hi.col <<- args$hi.col
    lo.col <<- args$lo.col
  },
  
  monthly = function() {
    require(xts)
    require(lubridate)
    data <<- data[, list(Date=last(eval(as.name(.time))), 
                         Open=first(eval(as.name(op.col))),
                         High=max(eval(as.name(hi.col))),
                         Low=min(eval(as.name(lo.col))),
                         Close=last(eval(as.name(cl.col)))),
                  by=list(eval(as.name(.id)), 
                          year(eval(as.name(.time))), 
                          month(eval(as.name(.time))))]
    setnames(data, "as.name", .id) # fixes bug above manually https://github.com/Rdatatable/data.table/issues/750
    data <<- data[, list(eval(as.name(.id)), Date, Open, High, Low, Close)]
    setkeyv(data, c(.id, "Date"))
    op.col <<- "Open"
    cl.col <<- "Close"
    hi.col <<- "High"
    lo.col <<- "Low"
    .freq <<- 12L
    return(self)
  },
  
  ClCl = function() {
    expr = as.call(parse(text="quantmod::ClCl(.SD)"))[[1]]
    require(quantmod) # TODO: remove quantmod dependency
    data[,Return:=eval(expr), by=.id] #TODO: optimize with .SDcols
    r = Returns$new(data=data,
                    id.col=.id,
                    time.col=.time,
                    val.col="Return")
    return(r)
  },
  
  returns = function() {
    return(ClCl())
  }
                )
)

ohlc <- function(x, val.cols=NULL) {
  o = OHLC$new(data=x,
               op.col=colnames(x)[grep("op", colnames(x), ignore.case=T)],
               cl.col=colnames(x)[grep("cl", colnames(x), ignore.case=T)],
               hi.col=colnames(x)[grep("hi", colnames(x), ignore.case=T)],
               lo.col=colnames(x)[grep("(?<!c)lo", 
                                       colnames(x), 
                                       ignore.case=T, 
                                       perl = T)])
  return(o)
}

prices <- function(x, val.col, ...) {
  p = Prices$new(data=x, val.col=val.col, ...)
  return(p)
}
                                     