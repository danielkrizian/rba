library(R6); library(data.table); library(xts)
OHLCV = readRDS("data/OHLCV.rds")
PRICES_wide = dcast.data.table(OHLCV[,list(Instrument, Date ,Close)], 
                             formula = Date ~ Instrument )

# detect tall/wide formats
detect_cols(OHLCV)
detect_cols(PRICES_wide)

# Monthly returns from OHLCV bars, calculated from Close to Close by default
R = ohlc(OHLCV)$monthly()$returns()

# monthly returns from Close prices
prices(OHLCV, val.col="Close")$monthly()$returns()

# construct from tall data format, specifying column of return values
returns(R$talldata, col="Return")


ohlc(OHLCV)$monthly() # monthly OHLC bars
ohlc(OHLCV)$monthly()$returns()$cor() # monthly return correlations 

ohlc(OHLCV)$monthly()$returns()$calendar()
