require(quantmod) 

getSymbols(c('0700.HK','0763.HK'))
A_ret=dailyReturn(`0700.HK`)
C_ret=dailyReturn(`0763.HK`)
ssdat=merge(A_ret,C_ret)
dim(ssdat)
ssdat=na.omit(ssdat)
dim(ssdat)


require(timeSeries)
dat=as.timeSeries(ssdat)
head(dat)
plot(dat)

library(fPortfolio) 

#模型设定
mvspec<-portfolioSpec()
setRiskFreeRate(mvspec)<-0
setSolver(mvspec)<-"solveRshortExact"
print(mvspec)
data<-100*(1+dat)
Data<-portfolioData(100*dat,mvspec)#100*LPP2005.RET一些股票的收益率
print(Data)
constrains<-"Short"
portfolioConstraints(data,mvspec,constrains)



#方差最小组合求解
globminportfolio<-minvariancePortfolio(Data,mvspec,constrains)
print(globminportfolio)

#求解特定组合的均值方差
m1vspec<-portfolioSpec()
data1<-100*dat
Data1<-portfolioData(100*dat,m1vspec)
n<-ncol(data1)
setWeights(m1vspec)<-rep(1/n,n)
m1vPortfolio<-feasiblePortfolio(Data1,m1vspec,constraints="LongOnly")
print(m1vPortfolio)

#在上面同等收益下，优化组合
mvspec1<-portfolioSpec()
setRiskFreeRate(mvspec1)<-0.05
targetReturn<-getTargetReturn(m1vPortfolio@portfolio)["mean"]
setTargetReturn(mvspec1)<-targetReturn
efficientportfolio<-efficientPortfolio(Data,spec=mvspec1)
weightsPie(efficientportfolio)
efficientportfolio

#做出有效前沿
data2<-100*dat
lppspec<-portfolioSpec()
setRiskFreeRate(lppspec)<-0.005
frontier<-portfolioFrontier(data2,lppspec)
#plot(frontier)
tailoredFrontierPlot(frontier)
frontierPlot(frontier)
cmlPoints(frontier,col=2)
frontier
weightsPlot(frontier)








## Use Swiss Pension Fund Data Set of Returns - 
   head(LPP2005REC)
   SPI <- LPP2005REC[, "SPI"]
   head(SPI)
   
## Plot Drawdowns - 
   rmean <- rollMean(SPI, k = 10)
   plot(rmean)