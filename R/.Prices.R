library(xts) # # TODO: remove quantmod dependency

##### value index ####

cumProd <- function(x, base) {
  firstLeadNonNA = xts:::naCheck(x)$beg
  x[!is.na(x)] <- base * cumprod( 1 + x[!is.na(x)] )
  if(firstLeadNonNA > 0L)
    x[firstLeadNonNA] = base
  x
}

#' Prices is a class for representing price data
#'
#'

prices <- function(x, benchmarks=NULL) {
  scale = periodicity(as.xts(x))$scale
  ann = switch(scale,
               "yearly"=1,
               "quarterly"=4,
               "monthly"=12,
               "weekly"=52,
               "daily"=252,
               "hourly"=252*8) # TODO: ann value for hourly data

  # fill in non-leading NAs with previous values
  x = xtsrunapply(x, function(col) na.locf(col, na.rm=F))

  obj = structure(x, class=c("prices", "xts", "zoo"), ann=ann)
  obj = set_benchmarks(obj, benchmarks)
  return(obj)

}

set_benchmarks <- function(x, ...) {
  if(!inherits(x, "xts")) stop("Cannot set benchmarks for non-xts objects")
  UseMethod("set_benchmarks")
}

set_benchmarks.default <- function(x, benchmarks) {
  benchmarks = c(xtsAttributes(x)$benchmarks, benchmarks)
  if(!is.null(benchmarks)) {
    assets = setdiff(colnames(x), benchmarks)
    if(!all(benchmarks %in% names(x)))
      stop("Some of the benchmark names not in the data.")
    x = x[,c(assets, benchmarks)] # put benchmarks to the end
    xtsAttributes(x) <- list(benchmarks=benchmarks)
  }
  return(x)
}

as.prices <- function(x, ...) UseMethod("as.prices")

as.prices.prices <- function(x, ...) x

as.prices.data.frame <- function(x, ...) {
  x = .as.xts.data.table(x)
  prices(x, ...)
}

#' Construct the chained value index from relative changes
#'
#' todo: speed-up parallel colwise cumprod with leading NAs
#' current horse race: http://codereview.stackexchange.com/questions/39180/best-way-to-apply-across-an-xts-object
as.prices.returns <- function(x, base = 100, ...) {
  pr = xtsrunapply(x, cumProd, base = base)
  pr = prices(pr)
  xtsAttributes(pr) = xtsAttributes(x, user=T)
  pr
}

# Ops.returns <- function(e1, e2) {
#   xts:::Ops.xts(as.xts(e1), as.xts(e2))
# }

# `[.prices` <- function(x, i, j, period) {
#
# }
