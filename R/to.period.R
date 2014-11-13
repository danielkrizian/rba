# custom function based on xts::to.period
# changes: na.omit(x) only if all(is.na(x)) - was any(is.na(x))
to.period = function (x, period = "months", k = 1, indexAt = NULL, name = NULL, 
                      OHLC = TRUE, ...) 
{
  if (missing(name)) 
    name <- deparse(substitute(x))
  xo <- x
  x <- try.xts(x)
  if (NROW(x) == 0 || NCOL(x) == 0) 
    stop(sQuote("x"), " contains no data")
  if (all(is.na(x))) {
    x <- na.omit(x)
    warning("missing values removed from data")
  }
  if (!OHLC) {
    xx <- x[endpoints(x, period, k), ]
  }
  else {
    if (!is.null(indexAt)) {
      index_at <- switch(indexAt, startof = TRUE, endof = FALSE, 
                         FALSE)
    }
    else index_at <- FALSE
    cnames <- c("Open", "High", "Low", "Close")
    if (has.Vo(x)) 
      cnames <- c(cnames, "Volume")
    if (has.Ad(x) && is.OHLC(x)) 
      cnames <- c(cnames, "Adjusted")
    cnames <- paste(name, cnames, sep = ".")
    if (is.null(name)) 
      cnames <- NULL
    xx <- .Call("toPeriod", x, endpoints(x, period, k), 
                has.Vo(x), has.Vo(x, which = TRUE), has.Ad(x) && 
                  is.OHLC(x), index_at, cnames, PACKAGE = "xts")
  }
  if (!is.null(indexAt)) {
    if (indexAt == "yearmon" || indexAt == "yearqtr") 
      indexClass(xx) <- indexAt
    if (indexAt == "firstof") {
      ix <- as.POSIXlt(c(.index(xx)), tz = indexTZ(xx))
      if (period %in% c("years", "months", "quarters", 
                        "days")) 
        index(xx) <- firstof(ix$year + 1900, ix$mon + 
                               1)
      else index(xx) <- firstof(ix$year + 1900, ix$mon + 
                                  1, ix$mday, ix$hour, ix$min, ix$sec)
    }
    if (indexAt == "lastof") {
      ix <- as.POSIXlt(c(.index(xx)), tz = indexTZ(xx))
      if (period %in% c("years", "months", "quarters", 
                        "days")) 
        index(xx) <- as.Date(lastof(ix$year + 1900, 
                                    ix$mon + 1))
      else index(xx) <- lastof(ix$year + 1900, ix$mon + 
                                 1, ix$mday, ix$hour, ix$min, ix$sec)
    }
  }
  reclass(xx, xo)
}
