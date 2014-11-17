library(R6); library(data.table); library(xts)
Sys.setenv(TZ="UTC") # options("xts_check_TZ"). print.xts causes xts:::check.TZ
OHLCV = readRDS("data/OHLCV.rds")
PRICES_wideod = dcast.data.table(OHLCV[,list(Instrument, Date ,Close)], 
                               formula = Date ~ Instrument )
RETURNS_wide = dcast.data.table(OHLCV[,list(Date,
                                            Returns=pch(Close, na.pad=F)), 
                                      by=Instrument], 
                                formula = Date ~ Instrument )
# detect tall/wide formats
# detect_cols(OHLCV)
# detect_cols(PRICES_wide)

#### construction ####
performance(returns(RETURNS_wide))$prices
p = prices(PRICES_wide)$monthly()
performance(p)$prices
performance(p)$returns

# TODO: test constructors from OHLC data
# Monthly returns from OHLCV bars, calculated from Close to Close by default
# R = ohlc(OHLCV)$monthly()$returns()
# prices(OHLCV, val.col="Close")$monthly()$returns() # monthly returns from Close prices
# ohlc(OHLCV)$monthly() # monthly OHLC bars
# ohlc(OHLCV)$monthly()$returns()$cor() # monthly return correlations 
# ohlc(OHLCV)$monthly()$returns()$calendar()

performance(p)$summary()


