# todo: speed-up parallel colwise apply
# current horse race: http://codereview.stackexchange.com/questions/39180/best-way-to-apply-across-an-xts-object
xtsrunapply <- function(x, FUN, ...) {
  "[<-"(x, , vapply(x, FUN, FUN.VALUE = numeric(nrow(x)), ...=...))
}

# returns single metric for each column. Takes care of leading NAs
summary.xts = function(x, FUN, ...) {
  vapply(x, function(col, ...) {
    FUN(col[xts:::naCheck(col)$nonNA], ...)
  }, FUN.VALUE = numeric(1), ...=...)
}

msapply <- function(...) {
  arglist <- match.call(expand.dots = FALSE)$...
  var.names <- sapply(arglist, deparse)
  has.name <- (names(arglist) != "")
  var.names[has.name] <- names(arglist)[has.name]
  arglist <- lapply(arglist, eval.parent, n = 2)
  x <- arglist[[1]]
  arglist[[1]] <- NULL
  result <- sapply(arglist, function (FUN, x) sapply(x, FUN), x)
  colnames(result) <- var.names[-1]
  return(result)
}

msum = function(x,...){
#   http://r.789695.n4.nabble.com/Applying-multiple-functions-to-one-object-td3254253.html
  fun.names = sapply(lapply(substitute(list(...)), deparse)[-1], paste, collapse="") 
  mthd<-list(...) 
  if(!is.list(x)) x = list(x) 
  res = t(sapply(x, function(y) sapply(mthd, function(m) do.call(m, list(y)) ))) 
  colnames(res) = fun.names 
  rownames(res) = names(x) 
  res 
} 


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
  
  id.col = get_rated_col(data, 
                         - istimebased + inkey + ischar + isfac + 0.5*isinteger
                         , min=1)
  time.col = get_rated_col(data, inkey + istimebased)
  val.col = get_rated_col(data, 
                          score= -inkey - istimebased - ischar + 
                            isnumeric + 5*isreturn)
  
  return(list(id.col=id.col, time.col=time.col, val.col=val.col))
}

as.xts.data.table <- function(x, index.col=NULL){
  if(is.null(index.col))
    index.col = detect_cols(x)$time.col
  if(is.symbol(index.col))
    index.col = deparse(substitute(index.col))
  order.by = as.Date(as.character(x[[index.col]]))
  x[, c(index.col):=NULL]
  out = xts(x, order.by = order.by)
  return(out)
}

##### percent change ####

pch <- function(x, na.pad = T) UseMethod("pch")

pch.default <- function(x, na.pad = T) {
  pch = diff.default(x)/x[2:length(x)]
  if(na.pad)
    c(NA, pch)
  return(pch)
}

pch.xts <- function(x, na.pad = T) {
  diff.xts(x, na.pad=na.pad)/tail(x, -1)
}

pch.list <- function(x, na.pad = T) {
  lapply(x, function(.XTS) pch.xts(.XTS))
}

cumProd = function(x, base) {
  firstLeadNonNA = xts:::naCheck(x)$beg
  x[!is.na(x)] <- base * cumprod( 1 + x[!is.na(x)] )
  if(firstLeadNonNA > 0L)
    x[firstLeadNonNA] = base
  x
}

#' Detect returns data
#' 
#' Returns TRUE/FALSE, with AIC of the distribution fit as an attribute 
#' Stub only, TODO: finalize
like.returns <- function(x){
  library(MASS)
  fitdistr(x$Performance, "normal")
  descdist(x$Performance)
  structure(TRUE, AIC=1)
}
