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
  ann = periodicity(as.xts(x))$frequency/60/60/24*12
  structure(x, class=c("prices", "xts", "zoo"), ann=ann, benchmarks=benchmarks)
}

as.prices <- function(x, ...) UseMethod("as.prices")

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
