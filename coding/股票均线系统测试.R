rm(list=ls())
require(quantstrat)
currency("RMB")
setSymbolLookup("北京银行"=list(name="601169.ss",src="yahoo"))
setSymbolLookup("平安银行"=list(name="000001.sz",src="yahoo"))
setSymbolLookup("民生银行"=list(name="600016.ss",src="yahoo"))
setSymbolLookup("招商银行"=list(name="600036.ss",src="yahoo"))
setSymbolLookup("农业银行"=list(name="601288.ss",src="yahoo"))
setSymbolLookup("交通银行"=list(name="601328.ss",src="yahoo"))
setSymbolLookup("兴业银行"=list(name="601166.ss",src="yahoo"))
setSymbolLookup("建设银行"=list(name="601939.ss",src="yahoo"))
setSymbolLookup("光大银行"=list(name="601818.ss",src="yahoo"))
setSymbolLookup("工商银行"=list(name="601398.ss",src="yahoo"))
setSymbolLookup("中信银行"=list(name="601998.ss",src="yahoo"))
setSymbolLookup("南京银行"=list(name="601009.ss",src="yahoo"))
setSymbolLookup("宁波银行"=list(name="002142.sz",src="yahoo"))
setSymbolLookup("浦发银行"=list(name="600000.ss",src="yahoo"))
setSymbolLookup("中国银行"=list(name="601988.ss",src="yahoo"))
setSymbolLookup("华夏银行"=list(name="600015.ss",src="yahoo"))
symbols=c("北京银行","平安银行","民生银行","招商银行",
"农业银行","交通银行","兴业银行","建设银行","光大银行",
"工商银行","中信银行","南京银行","宁波银行","中国银行",
"浦发银行","华夏银行")
beginTime=as.Date("2007-01-01")
endTime=as.Date("2015-04-28")
initEq <- 1e6
Sys.setenv(TZ="UTC")
getSymbols(symbols,from=beginTime,to=endTime,index.class=c("POSIXt","POSIXct"),adjust=T)
for(symbol in symbols)
{
stock(symbol, currency="RMB",multiplier=1)
x<-get(symbol)
x<-to.monthly(x,indexAt='endof',drop.time=FALSE)
indexFormat(x)<-'%Y-%m-%d'
colnames(x)<-gsub("x",symbol,colnames(x))
assign(symbol,x)
}
  x==to.monthly(x,indexAt='endof', drop.time=FALSE)
x$SMA10=SMA(Ad(x),10)
 qs.strategy <- "云金杞的策略"
initPortf(qs.strategy,'x',initDate = beginTime)
initAcct(qs.strategy,portfolios=qs.strategy, initDate=beginTime,initEq=initEq)
initOrders(portfolio=qs.strategy,initDate = beginTime)
strategy(qs.strategy,store=TRUE)
strat <-getStrategy(qs.strategy)
class(strat)
summary(strat)
add.indicator(strategy = qs.strategy, name = "SMA",
arguments = list(x = quote(Cl(mktdata)), n=10), label="SMA10")
 summary(getStrategy(qs.strategy))
 add.signal(qs.strategy,name="sigCrossover",
arguments = list(columns=c("Close","SMA10"),relationship="gt"),
label="Cl.gt.SMA")
add.signal(qs.strategy,name="sigCrossover",
arguments = list(columns=c("Close","SMA10"),relationship="lt"),
label="Cl.lt.SMA")
add.rule(qs.strategy, name='ruleSignal',
arguments = list(sigcol="Cl.gt.SMA", sigval=TRUE, orderqty=900,
ordertype='market', orderside='long'),
type='enter')
add.rule(qs.strategy, name='ruleSignal',
arguments = list(sigcol="Cl.lt.SMA", sigval=TRUE, orderqty='all',
ordertype='market', orderside='long'),
type='exit')
multi.asset <- "multiAsset"
 rm.strat(multi.asset)
 initPortf(multi.asset,symbols=symbols, initDate=beginTime)
initAcct(multi.asset,portfolios=multi.asset, initDate=beginTime,initEq=initEq)
initOrders(portfolio=multi.asset,initDate=beginTime)
 applyStrategy(strategy=qs.strategy , portfolios=multi.asset)
 updatePortf(multi.asset)
 updateAcct(multi.asset)
 updateEndEq(multi.asset)
 checkBlotterUpdate <- function(port.st,account.st,verbose=TRUE)
{
ok <- TRUE
p <- getPortfolio(port.st)
a <- getAccount(account.st)
syms <- names(p$symbols)
port.tot <- sum(sapply(syms,FUN = function(x) eval(parse(
text=paste("sum(p$symbols",x,"posPL.USD$Net.Trading.PL)",sep="$")))))
port.sum.tot <- sum(p$summary$Net.Trading.PL)
if( !isTRUE(all.equal(port.tot,port.sum.tot)) ) {
ok <- FALSE
if( verbose )
print("portfolio P&L doesn't match sum of symbols P&L")
}
initEq <- as.numeric(first(a$summary$End.Eq))
endEq <- as.numeric(last(a$summary$End.Eq))
if( !isTRUE(all.equal(port.tot,endEq-initEq)) ) {
ok <- FALSE
if( verbose )
print("portfolio P&L doesn't match account P&L")
}
if( sum(duplicated(index(p$summary))) ) {
ok <- FALSE
if( verbose )
print("duplicate timestamps in portfolio summary")
}
if( sum(duplicated(index(a$summary))) ) {
ok <- FALSE
if( verbose )
print("duplicate timestamps in account summary")
}
return(ok)
}
checkBlotterUpdate(multi.asset,multi.asset)
 a <- getAccount(multi.asset)
 p <- getPortfolio(multi.asset)
 names(p$symbols)
 myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
par(mfrow=c(4,4))
for(symbol in symbols)
{
chart.Posn(Portfolio=multi.asset,Symbol=symbol,theme=myTheme,
TA="add_SMA(n=10,col='blue')")
}
par(mfrow=c(1,1))
rets.multi <- PortfReturns(multi.asset)
colnames(rets.multi) <- symbols
rets.multi <- na.omit(cbind(rets.multi,Return.calculate(a$summary$End.Eq)))
names(rets.multi)[length(names(rets.multi))] <- "TOTAL"
rets.multi <- rets.multi[,c("TOTAL",symbols)]
round(tail(rets.multi,5),6)
chart.CumReturns(rets.multi, colorset= rich10equal, legend.loc = "topleft",
main="SPDR Cumulative Returns")
(ar.tab <- table.AnnualizedReturns(rets.multi))
max.risk <- max(ar.tab["Annualized Std Dev",])
max.return <- max(ar.tab["Annualized Return",])
chart.RiskReturnScatter(rets.multi,
main = "SPDR Performance", colorset = rich10equal,
xlim=c(0,max.risk*1.1),ylim=c(0,max.return))
equity <- a$summary$End.Eq
plot(equity,main="Consolidated SPDR Equity Curve")

 





 
 




