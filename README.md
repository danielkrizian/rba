rba
===

Return-based Analysis - manipulate and analyze time-series objects of returns 
and prices.

http://vita.had.co.nz/papers/mutatr.pdf

the difference between object-oriented programming (hence I use R6 Classes 
for this project) versus functional programming

```r
file$open()$read_lines(1:5)$replace(" ", "-")
replace(read_lines(open(file), 1:5), " ", "-")
```

first one feels more natural. less parentheses. first is object oriented, second 
is function oriented. file there is object retaining its state so it is very 
useful to model objects like portfolio or returns structure.
Allows us to write something like:

```r
# Multi-instrument prices at Open, High, Low and Close
OHLCV = readRDS("data/OHLCV.rds") 

# monthly return correlations
ohlc(OHLCV)$monthly()$returns()$cor()
```