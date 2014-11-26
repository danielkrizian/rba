library(R6); library(data.table); library(xts)
Sys.setenv(TZ="UTC") # options("xts_check_TZ"). print.xts causes xts:::check.TZ
OHLCV = readRDS("data/OHLCV.rds")
PRICES_wide = dcast.data.table(OHLCV[,list(Instrument, Date ,Close)],
                               formula = Date ~ Instrument )
RETURNS_wide = dcast.data.table(OHLCV[,list(Date,
                                            Returns=pch(Close, na.pad=F)),
                                      by=Instrument],
                                formula = Date ~ Instrument )
# detect tall/wide formats
# detect_cols(OHLCV)
# detect_cols(PRICES_wide)

#### construction ####
r = as.returns(RETURNS_wide, benchmarks="SPX")
p = as.prices(PRICES_wide,  benchmarks="SPX")

# compress into monthly frequency
r = to.monthly(r)
p = to.monthly(p)

# TODO:  OHLC class constructor
# Monthly returns from OHLCV bars, calculated from Close to Close by default
# R = ohlc(OHLCV)$monthly()$returns()

do.call("cbind",c(
calculate(trailing(r, "mtd"), "performance"),
calculate(trailing(r, "qtd"), "performance"),
calculate(trailing(r, "ytd"), "performance"),
calculate(trailing(r, "l12m"), "performance")))

cor(r)
capm(trailing(r, "l12m"))$Alpha

calculate(annualized(trailing(r, "l12m")), "sd")
capm(annualized(trailing(r, "l12m")))

calculate(annualized(trailing(r, "mtd")), "performance")


portf = portfolio(r, weights=c(VTI=0.5, DBC=0.5))
summary(portf)

# capm(trailing(r, "l12m"))
