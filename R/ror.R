as.ror <- function (x, ...) {
  UseMethod("as.ror")
}

as.ror.xts = function(x){
  class(x) = c("ror", class(x))
  pertable = c(minute="minute", hourly="hour", daily="day", weekly="week", monthly="month", quarterly="quarter", yearly="year")
  attr(x,"periodicity") = pertable[xts::periodicity(x)$scale]
  x
}

ror <- function(x, geometric = geometric, na.rm = TRUE) {if (geometric) prod(1 + x, na.rm=na.rm) - 1 else sum(x, na.rm=na.rm)}
format.percent <- function(x, digits) {ifelse(is.na(x), format("",width = 7), paste0(format(round(100*x, digits), nsmall = digits), "%"))}

calendar <- function(x, ...) {
  UseMethod("calendar")
}

calendar.xts <- function(x, ...) {
  calendar.ror(as.ror(x), ...)
}

calendar.ror <- function(x, bm=2:ncol(x), bm.compare=c("year", "month"),
                          fmt=c("raw", "percent"), desc=TRUE, digits = 2, geometric = TRUE)
{
  require(data.table)
  bm.compare = ifelse(length(bm), bm.compare[1], "none")
  fmt = fmt[1]
  
  if (!attr(x,"periodicity") == "month")
    stop("Non-monthly periodicity not yet implemented.")
  
  if (bm.compare == "month")
    stop("bm=='month': Comparison with benchmark by month not yet implemented.")
  
  
  assets = colnames(x)
  if (length(bm)) {
    assets = assets[-bm]
  }
  benchmarks = colnames(x)[bm]
  x = data.table:::as.data.table.xts(x)
  
  ytd = data.table::melt.data.table(x, measure.vars = 2:ncol(x), variable.name = "Asset", value.name = "Return")
  ytd = ytd[ , list(YTD = ror(Return, geometric)),by = list(Asset, year(index))]
  ytd = setkey(data.table::dcast.data.table(ytd, year ~ Asset, value.var = "YTD", fill = NA), year)
  
  x[, c("month","year") := list(month(index), year(index))]
  
  year.by.month.ytd <- function(asset) {
    # no_leading_NAs = x[min(which(!is.na(x[[asset]]))):nrow(x)]
    tbl = data.table::dcast.data.table(x, year ~ month, fun.aggregate = ror, geometric = geometric, na.rm=FALSE, fill = NA, value.var = asset)
    tbl = tbl[which(apply(is.na(tbl[,-1, with=FALSE]), 1, sum)!=12)] # remove empty years
    tbl = setkey(tbl, year)[ytd[, c("year", asset), with = FALSE], nomatch=0]
    if (bm.compare == "year")
      tbl = setkey(tbl, year)[ytd[, c("year", benchmarks), with = FALSE]]
    if (desc)
      tbl = tbl[order(-year)]
    
    tbl = as.data.frame(tbl)
    rownames(tbl) = tbl[, 1]
    tbl = tbl[, -1]
    month.labels = strftime(seq.Date(as.Date("2000-01-01"), length.out = 12, by = "months"), format = "%b")
    colnames(tbl)[1:13] = c(month.labels, "YTD")
    if (fmt == "percent")
      tbl = apply(tbl, 2, FUN = format.percent, digits = digits)
    tbl
  }

  out = sapply(assets, year.by.month.ytd, simplify = F)
  
  if (length(out) == 1)
    out = out[[1]]
  return(out)
}

print.ror <- function(x) {
  cal = calendar(x, bm = NULL, fmt = "percent")
  if(!is.list(cal))
    if (nrow(cal) > 20) {
      print( head(cal, 10), digits = 4, right = TRUE, quote = F)
      cat("...\n")
      print( tail(cal, 10), digits = 4, right = TRUE, quote = F)
    }
  print( cal, digits = 4, right = TRUE, quote = F)
}
