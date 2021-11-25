
#投资组合研究
stock = read.csv(file.choose(),header = TRUE) #读入股票代码表
tickers = paste(stock$Code, "ss", sep=".")    #整理股票代码表

#下载股票数据
library(quantmod)
setSymbolLookup(SSE1=list(name=tickers[1],src='yahoo'))	#代码=》名字缩写
setSymbolLookup(SSE2=list(name=tickers[2],src='yahoo'))	
setSymbolLookup(SSE3=list(name=tickers[3],src='yahoo'))	
setSymbolLookup(SSE4=list(name=tickers[4],src='yahoo'))
setSymbolLookup(SSE5=list(name=tickers[5],src='yahoo'))
getSymbols(c("SSE1","SSE2","SSE3","SSE4","SSE5"),from = "2014-01-01",to = Sys.Date(),
            src = "yahoo",auto.assign=TRUE)
head(SSE2)
RET1=dailyReturn(Ad(SSE1))   #日收益率
RET2=dailyReturn(Ad(SSE2))  
RET3=dailyReturn(Ad(SSE3)) 
RET4=dailyReturn(Ad(SSE4)) 
RET5=dailyReturn(Ad(SSE5)) 
RET=merge(RET1, RET2,RET3, RET4,RET5)
colnames(RET)=c("SSE1","SSE2","SSE3","SSE4","SSE5")
head(RET)
##########################################################
#                                                        #    
#                                                        #
#                                                        #
#                                                        #
#                                                        #
##########################################################

RET = read.csv(file.choose(),header = TRUE) #读入股票
dim(RET)
RET=na.omit(RET)
SSE50RET=xts(RET[,c(1,3,5)], as.Date(RET[,1], format='%Y/%m/%d'))

SSE50RET=as.timeSeries(SSE50RET)

plot(SSE50RET[,1:5])

#股票收益表现
library(PerformanceAnalytics)
charts.PerformanceSummary(SSE50RET,main = "上证50各股表现") #表现列表
rbind(table.AnnualizedReturns(RET), maxDrawdown(RET)) 
apply.yearly(RET, Return.cumulative)

#组合模型设定
library(fPortfolio) 

MVspec <- portfolioSpec(portfolio=list(targetReturn=0.3)) # 设定组合的期望收益率
cons <- 'LongOnly' # 设定组合的约束不许做空
setRiskFreeRate(MVspec)<-0.025 # 设定无风险利率

RET= as.timeSeries(RET)
data<-portfolioData(100*RET,MVspec)#对数据乘上100是为了转换为收益率

#做出有效前沿
frontier<-portfolioFrontier(data,MVspec)
tailoredFrontierPlot(frontier)
plot(frontier)
#frontierPlot(frontier)
#cmlPoints(frontier,col=2)
frontier
weightsPlot(frontier)



#模型求解
Portfolio1=efficientPortfolio(data, spec = portfolioSpec(), constraints = "LongOnly")
Portfolio2=maxratioPortfolio(data, spec = portfolioSpec(), constraints = "LongOnly")
Portfolio3=tangencyPortfolio(data, spec = portfolioSpec(), constraints = "LongOnly")
Portfolio4=minriskPortfolio(data, spec = portfolioSpec(), constraints = "LongOnly")
Portfolio5=minvariancePortfolio(data, spec = portfolioSpec(), constraints = "LongOnly")

spec6=portfolioSpec()
setTargetRisk(spec6)<-0.1 # 设定目标风险利率
Portfolio6=maxreturnPortfolio(data, spec = spec6, constraints = "LongOnly")

par(mfrow=c(2,3))

weightsPie(Portfolio1)
weightsPie(Portfolio2)
weightsPie(Portfolio3)
weightsPie(Portfolio4)
weightsPie(Portfolio5)
weightsPie(Portfolio6)



getWeights(Portfolio1)


stratRets <- Return.portfolio(R = RET, weights = getWeights(Portfolio2))
rbind(table.AnnualizedReturns(stratRets), maxDrawdown(stratRets))
stratAndComponents <- merge(RET, stratRets, join='inner')
charts.PerformanceSummary(stratAndComponents)
rbind(table.AnnualizedReturns(stratAndComponents ), maxDrawdown(stratAndComponents ))
apply.yearly(stratRets, Return.cumulative)





require(stockPortfolio)
require(PerformanceAnalytics)
library(fPortfolio) 

stock=read.csv(file.choose(),header = TRUE)
stock$Code=paste(stock$Code, "ss",sep=".")
head(tickers)

SSE50 <- getReturns(stock$Code[1:5], start='2008-01-01', end='2014-06-30')
SSE50$full
returns=SSE50$R
SSE50RET=as.timeSeries(returns)
charts.PerformanceSummary(SSE50RET)

stratRets <- Return.portfolio(R = SSE50RET, weights = c( 0.0000,0.0946,0.3212,0.3229,0.2613 ))
rbind(table.AnnualizedReturns(stratRets), maxDrawdown(stratRets))
stratAndComponents <- merge(SSE50RET, stratRets, join='inner')
charts.PerformanceSummary(stratAndComponents)
rbind(table.AnnualizedReturns(stratAndComponents ), maxDrawdown(stratAndComponents ))
apply.yearly(stratRets, Return.cumulative)



#模型设定
mvspec<-portfolioSpec()
setRiskFreeRate(mvspec)<-0
setSolver(mvspec)<-"solveRshortExact"
print(mvspec)

Data<-portfolioData(100*SSE50RET,mvspec)#对数据乘上100是为了转换为收益率
print(Data)
constrains<-"Short"
data<-100*(1+SSE50RET)
portfolioConstraints(data,mvspec,constrains)



#方差最小组合求解
globminportfolio<-minvariancePortfolio(Data,mvspec,constrains)
print(globminportfolio)

#求解特定组合的均值方差
m1vspec<-portfolioSpec()
data1<-100*(1+SSE50RET)
Data1<-portfolioData(100*SSE50RET,m1vspec)
n<-ncol(data1)
setWeights(m1vspec)<-rep(1/n,n)
m1vPortfolio<-feasiblePortfolio(Data1,m1vspec,constraints="LongOnly")
print(m1vPortfolio)

#在上面同等收益下，优化组合
mvspec1<-portfolioSpec()
setRiskFreeRate(mvspec1)<-0.05
targetReturn<-getTargetReturn(m1vPortfolio@portfolio)["mean"]
setTargetReturn(mvspec1)<-targetReturn
efficientportfolio<-efficientPortfolio(Data1,spec=mvspec1)
weightsPie(efficientportfolio)
efficientportfolio

#做出有效前沿
data2<-100*SSE50RET
lppspec<-portfolioSpec()
setRiskFreeRate(lppspec)<-0.005
frontier<-portfolioFrontier(data2,lppspec)
#plot(frontier)
tailoredFrontierPlot(frontier)
frontierPlot(frontier)
cmlPoints(frontier,col=2)
frontier
weightsPlot(frontier)





plot(100*SSE50RET)








require(xts)
require(zoo)
require(quantmod)


stock=read.csv(file.choose(),header = TRUE)
stock$Code=paste(stock$Code, "ss",sep=".")
head(stock)
Codename=as.character(stock$Codename)
ssec<-getReturns('^SSEC', freq = "month",get = "all", start = "2008-01-01", end = NULL)
setSymbolLookup(SSE01=list(name=stock[1,1],src='yahoo'))	#	日照港	
setSymbolLookup(SSE02=list(name=stock[2,1],src='yahoo'))	#	上港集团	
setSymbolLookup(SSE03=list(name=stock[3,1],src='yahoo'))	#	中海发展	
setSymbolLookup(SSE04=list(name=stock[4,1],src='yahoo'))	#	中国石化
setSymbolLookup(SSE05=list(name=stock[5,1],src='yahoo'))	#	三一重工	
setSymbolLookup(SSE06=list(name=stock[6,1],src='yahoo'))	#	葛洲坝	
setSymbolLookup(SSE07=list(name=stock[7,1],src='yahoo'))	#	特变电工	
setSymbolLookup(SSE08=list(name=stock[8,1],src='yahoo'))	#	铁龙物流	
setSymbolLookup(SSE09=list(name=stock[9,1],src='yahoo'))	#	中国船舶	
setSymbolLookup(SSE10=list(name=stock[10,1],src='yahoo'))	#	锦州港	
setSymbolLookup(SSE11=list(name=stock[11,1],src='yahoo'))	#	广汇能源	
setSymbolLookup(SSE12=list(name=stock[12,1],src='yahoo'))	#	重庆港九	
setSymbolLookup(SSE13=list(name=stock[13,1],src='yahoo'))	#	平高电气	
setSymbolLookup(SSE14=list(name=stock[14,1],src='yahoo'))	#	营口港	
setSymbolLookup(SSE15=list(name=stock[15,1],src='yahoo'))	#	振华重工	
setSymbolLookup(SSE16=list(name=stock[16,1],src='yahoo'))	#	五洲交通	
setSymbolLookup(SSE17=list(name=stock[17,1],src='yahoo'))	#	国电南瑞	
setSymbolLookup(SSE18=list(name=stock[18,1],src='yahoo'))	#	青松建化	
setSymbolLookup(SSE19=list(name=stock[19,1],src='yahoo'))	#	中远航运	
setSymbolLookup(SSE20=list(name=stock[20,1],src='yahoo'))	#	晋西车轴	
setSymbolLookup(SSE21=list(name=stock[21,1],src='yahoo'))	#	中天科技	
setSymbolLookup(SSE22=list(name=stock[22,1],src='yahoo'))	#	中铁二局	
setSymbolLookup(SSE23=list(name=stock[23,1],src='yahoo'))	#	新疆城建	
setSymbolLookup(SSE24=list(name=stock[24,1],src='yahoo'))	#	卧龙电气	
setSymbolLookup(SSE25=list(name=stock[25,1],src='yahoo'))	#	海油工程	
setSymbolLookup(SSE26=list(name=stock[26,1],src='yahoo'))	#	天津港	
setSymbolLookup(SSE27=list(name=stock[27,1],src='yahoo'))	#	宁波海运	
setSymbolLookup(SSE28=list(name=stock[28,1],src='yahoo'))	#	隧道股份	
setSymbolLookup(DFDQ=list(name=stock[29,1],src='yahoo'))	#	东方电气	
setSymbolLookup(ZCGJ=list(name=stock[30,1],src='yahoo'))	#	中材国际	
setSymbolLookup(JYSY=list(name=stock[31,1],src='yahoo'))	#	晋亿实业	
setSymbolLookup(LYG=list(name=stock[32,1],src='yahoo'))	#	连云港	
setSymbolLookup(NBG=list(name=stock[33,1],src='yahoo'))	#	宁波港	
setSymbolLookup(ZGYZ=list(name=stock[34,1],src='yahoo'))	#	中国一重	
setSymbolLookup(ZGHX=list(name=stock[35,1],src='yahoo'))	#	中国化学	
setSymbolLookup(ZGXD=list(name=stock[36,1],src='yahoo'))	#	中国西电	
setSymbolLookup(ZGTJ=list(name=stock[37,1],src='yahoo'))	#	中国铁建	
setSymbolLookup(ZGZT=list(name=stock[38,1],src='yahoo'))	#	中国中铁	
setSymbolLookup(ZGZZ=list(name=stock[39,1],src='yahoo'))	#	中国中冶	
setSymbolLookup(ZGJZ=list(name=stock[40,1],src='yahoo'))	#	中国建筑	
setSymbolLookup(ZGDJ=list(name=stock[41,1],src='yahoo'))	#	中国电建	
setSymbolLookup(SHDQ=list(name=stock[42,1],src='yahoo'))	#	上海电气	
setSymbolLookup(ZGNC=list(name=stock[43,1],src='yahoo'))	#	中国南车	
setSymbolLookup(ZGJJ=list(name=stock[44,1],src='yahoo'))	#	中国交建	
setSymbolLookup(ZHYF=list(name=stock[45,1],src='yahoo'))	#	中海油服	
setSymbolLookup(ZGSY=list(name=stock[46,1],src='yahoo'))	#	中国石油	
setSymbolLookup(ZHJY=list(name=stock[47,1],src='yahoo'))	#	中海集运	
setSymbolLookup(ZSLC=list(name=stock[48,1],src='yahoo'))	#	招商轮船	
setSymbolLookup(DLG=list(name=stock[49,1],src='yahoo'))	#	大连港	
setSymbolLookup(SSE50=list(name=stock[50,1],src='yahoo'))	#	中国重工	
	

getSymbols(Codename[1:20],from = "2015-01-01",to = Sys.Date(),src = "yahoo",auto.assign=TRUE)
SSE_50=cbind(SSE01,SSE02,SSE03,SSE04,SSE05,SSE06,SSE07,SSE08,SSE09,SSE10,
SSE11,SSE12,SSE13,SSE14,SSE15,SSE16,SSE17,SSE18,SSE19,SSE20,
SSE21,SSE22,SSE23,SSE24,SSE25,SSE26,SSE27,SSE28,SSE29,SSE30,
SSE31,SSE32,SSE33,SSE34,SSE35,SSE36,SSE37,SSE38,SSE39,SSE40,
SSE41,SSE42,SSE43,SSE44,SSE45,SSE46,SSE47,SSE48,SSE49,SSE50)


SSE_50=as.data.frame(SSE_50)

CLOSE=SSE_50[,seq(4,312,6)]
CLOSE=as.data.frame(CLOSE)

write.csv(SSE_50, file = "E:上证50股票数据.csv", row.names = T, quote = F)
write.csv(CLOSE, file = "E:上证50收盘价.csv", row.names = T, quote = F)

require(quantmod)
require(PerformanceAnalytics)
setSymbolLookup(GZMT=list(name ="600519.SS", src='yahoo'))  #   贵州茅台
setSymbolLookup(MSYH=list(name ="600016.SS", src='yahoo'))  #   民生银行
getSymbols(c("GZMT", "MSYH"), from="2009-01-01")
returns <- merge(Return.calculate(Ad(GZMT)), Return.calculate(Ad(MSYH)), join='inner')
returns <- returns[-1,]


charts.PerformanceSummary(returns)
configs <- list()
for(i in 1:21) {
  weightSPY <- (i-1)*.05
  weightTLT <- 1-weightSPY
  config <- Return.portfolio(R = returns, weights=c(weightSPY, weightTLT), rebalance_on = "months")
  configs[[i]] <- config
}
configs <- do.call(cbind, configs)
cumRets <- cumprod(1+configs)


getSymbols(stock[2,1],from = "2015-04-01",to = Sys.Date(),src = "yahoo",auto.assign=FALSE, env = new.environment)