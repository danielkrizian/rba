#' returns list of column names representing id, time, value
detect_cols = function(data){
  
  timeBased = function(x) {
    if (!any(sapply(c("Date", "POSIXt", "chron",
                      "dates", "times",
                      "timeDate", "yearmon",
                      "yearqtr", "xtime"), function(xx) inherits(x, xx)))) {
      FALSE
    } else TRUE
  }
  
  get_rated_col = function(data, score, min=-Inf) {
    score = as.numeric(score)
    top = names(data)[score == max(score) & score >= min]
    if(!length(top))
      return(NULL)
    else
      top
  }
  
  inkey = names(data) %in% key(data)
  ischar = vapply(data, is.character, 1, USE.NAMES=F)
  isfac = vapply(data, is.factor, 1, USE.NAMES=F)
  istimebased= vapply(data, timeBased, 1, USE.NAMES=F)
  isnumeric = vapply(data, is.numeric, 1, USE.NAMES=F)
  isinteger = vapply(data, is.integer, 1, USE.NAMES=F)
  isreturn = vapply(names(data), grepl, 1, USE.NAMES=F, pattern="ret", 
                    ignore.case=TRUE)
  
  id.col = get_rated_col(data, inkey + ischar + isfac + 0.5*isinteger, min=1)
  time.col = get_rated_col(data, inkey + istimebased)
  val.col = get_rated_col(data, 
                          score= -inkey - istimebased - ischar + 
                            isnumeric + 5*isreturn)
  
  return(list(id.col=id.col, time.col=time.col, val.col=val.col))
}