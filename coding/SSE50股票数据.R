#投资组合研究
stock = read.csv(file.choose(),header = TRUE) #读入股票代码表
tickers = paste(stock$Code, "ss", sep=".")    #整理股票代码表

#下载股票数据
library(quantmod)
setSymbolLookup(INDEX=list(name="000001.ss",src='yahoo'))	#代码=》名字缩写
setSymbolLookup(SSE01=list(name=tickers[1],src='yahoo'))	#	浦发银行
setSymbolLookup(SSE02=list(name=tickers[2],src='yahoo'))	#	包钢股份
setSymbolLookup(SSE03=list(name=tickers[3],src='yahoo'))	#	华夏银行
setSymbolLookup(SSE04=list(name=tickers[4],src='yahoo'))	#	民生银行
setSymbolLookup(SSE05=list(name=tickers[5],src='yahoo'))	#	宝钢股份
setSymbolLookup(SSE06=list(name=tickers[6],src='yahoo'))	#	中国石化
setSymbolLookup(SSE07=list(name=tickers[7],src='yahoo'))	#	中信证券
setSymbolLookup(SSE08=list(name=tickers[8],src='yahoo'))	#	三一重工
setSymbolLookup(SSE09=list(name=tickers[9],src='yahoo'))	#	招商银行
setSymbolLookup(SSE10=list(name=tickers[10],src='yahoo'))	#	保利地产
setSymbolLookup(SSE11=list(name=tickers[11],src='yahoo'))	#	中国联通
setSymbolLookup(SSE12=list(name=tickers[12],src='yahoo'))	#	上汽集团
setSymbolLookup(SSE13=list(name=tickers[13],src='yahoo'))	#	包钢稀土
setSymbolLookup(SSE14=list(name=tickers[14],src='yahoo'))	#	兰花科创
setSymbolLookup(SSE15=list(name=tickers[15],src='yahoo'))	#	广汇能源
setSymbolLookup(SSE16=list(name=tickers[16],src='yahoo'))	#	阳泉煤业
setSymbolLookup(SSE17=list(name=tickers[17],src='yahoo'))	#	江西铜业
setSymbolLookup(SSE18=list(name=tickers[18],src='yahoo'))	#	金地集团
setSymbolLookup(SSE19=list(name=tickers[19],src='yahoo'))	#	中金黄金
setSymbolLookup(SSE20=list(name=tickers[20],src='yahoo'))	#	康美药业
setSymbolLookup(SSE21=list(name=tickers[21],src='yahoo'))	#	贵州茅台
setSymbolLookup(SSE22=list(name=tickers[22],src='yahoo'))	#	山东黄金
setSymbolLookup(SSE23=list(name=tickers[23],src='yahoo'))	#	厦门钨业
setSymbolLookup(SSE24=list(name=tickers[24],src='yahoo'))	#	海螺水泥
setSymbolLookup(SSE25=list(name=tickers[25],src='yahoo'))	#	海通证券
setSymbolLookup(SSE26=list(name=tickers[26],src='yahoo'))	#	伊利股份
setSymbolLookup(SSE27=list(name=tickers[27],src='yahoo'))	#	招商证券
setSymbolLookup(SSE28=list(name=tickers[28],src='yahoo'))	#	大秦铁路
setSymbolLookup(SSE29=list(name=tickers[29],src='yahoo'))	#	中国神华
setSymbolLookup(SSE30=list(name=tickers[30],src='yahoo'))	#	兴业银行
setSymbolLookup(SSE31=list(name=tickers[31],src='yahoo'))	#	北京银行
setSymbolLookup(SSE32=list(name=tickers[32],src='yahoo'))	#	农业银行
setSymbolLookup(SSE33=list(name=tickers[33],src='yahoo'))	#	中国北车
setSymbolLookup(SSE34=list(name=tickers[34],src='yahoo'))	#	中国平安
setSymbolLookup(SSE35=list(name=tickers[35],src='yahoo'))	#	交通银行
setSymbolLookup(SSE36=list(name=tickers[36],src='yahoo'))	#	新华保险
setSymbolLookup(SSE37=list(name=tickers[37],src='yahoo'))	#	工商银行
setSymbolLookup(SSE38=list(name=tickers[38],src='yahoo'))	#	中国太保
setSymbolLookup(SSE39=list(name=tickers[39],src='yahoo'))	#	中国人寿
setSymbolLookup(SSE40=list(name=tickers[40],src='yahoo'))	#	中国建筑
setSymbolLookup(SSE41=list(name=tickers[41],src='yahoo'))	#	中国水电
setSymbolLookup(SSE42=list(name=tickers[42],src='yahoo'))	#	华泰证券
setSymbolLookup(SSE43=list(name=tickers[43],src='yahoo'))	#	潞安环能
setSymbolLookup(SSE44=list(name=tickers[44],src='yahoo'))	#	中国南车
setSymbolLookup(SSE45=list(name=tickers[45],src='yahoo'))	#	光大证券
setSymbolLookup(SSE46=list(name=tickers[46],src='yahoo'))	#	光大银行
setSymbolLookup(SSE47=list(name=tickers[47],src='yahoo'))	#	中国石油
setSymbolLookup(SSE48=list(name=tickers[48],src='yahoo'))	#	紫金矿业
setSymbolLookup(SSE49=list(name=tickers[49],src='yahoo'))	#	方正证券
setSymbolLookup(SSE50=list(name=tickers[50],src='yahoo'))	#	中国重工

getSymbols(c("INDEX","SSE01","SSE02","SSE03","SSE04","SSE05","SSE06","SSE07","SSE08","SSE09","SSE10",
             "SSE11","SSE12","SSE13","SSE14","SSE15","SSE16","SSE17","SSE18","SSE19","SSE20",
             "SSE21","SSE22","SSE23","SSE24","SSE25","SSE26","SSE27","SSE28","SSE29","SSE30",
             "SSE31","SSE32","SSE33","SSE35","SSE36","SSE37","SSE38","SSE39","SSE40",
             "SSE41","SSE42","SSE43","SSE44","SSE45","SSE46","SSE47","SSE48","SSE49","SSE50"),
           from = "2014-01-01",to = "2015-12-31",src = "yahoo",auto.assign=TRUE)

RETIN=dailyReturn(Ad(INDEX))   #日收益率
RET01=dailyReturn(Ad(SSE01))  #日收益率
RET02=dailyReturn(Ad(SSE02)) 
RET03=dailyReturn(Ad(SSE03)) 
RET04=dailyReturn(Ad(SSE04)) 
RET05=dailyReturn(Ad(SSE05)) 
RET06=dailyReturn(Ad(SSE06)) 
RET07=dailyReturn(Ad(SSE07)) 
RET08=dailyReturn(Ad(SSE08)) 
RET09=dailyReturn(Ad(SSE09)) 
RET10=dailyReturn(Ad(SSE10)) 
RET11=dailyReturn(Ad(SSE11)) 
RET12=dailyReturn(Ad(SSE12)) 
RET13=dailyReturn(Ad(SSE13)) 
RET14=dailyReturn(Ad(SSE14)) 
RET15=dailyReturn(Ad(SSE15)) 
RET16=dailyReturn(Ad(SSE16)) 
RET17=dailyReturn(Ad(SSE17)) 
RET18=dailyReturn(Ad(SSE18)) 
RET19=dailyReturn(Ad(SSE19)) 
RET20=dailyReturn(Ad(SSE20)) 
RET21=dailyReturn(Ad(SSE21)) 
RET22=dailyReturn(Ad(SSE22)) 
RET23=dailyReturn(Ad(SSE23)) 
RET24=dailyReturn(Ad(SSE24)) 
RET25=dailyReturn(Ad(SSE25)) 
RET26=dailyReturn(Ad(SSE26)) 
RET27=dailyReturn(Ad(SSE27)) 
RET28=dailyReturn(Ad(SSE28)) 
RET29=dailyReturn(Ad(SSE29)) 
RET30=dailyReturn(Ad(SSE30)) 
RET31=dailyReturn(Ad(SSE31)) 
RET32=dailyReturn(Ad(SSE32)) 
RET33=dailyReturn(Ad(SSE33)) 
 
RET35=dailyReturn(Ad(SSE35)) 
RET36=dailyReturn(Ad(SSE36)) 
RET37=dailyReturn(Ad(SSE37)) 
RET38=dailyReturn(Ad(SSE38)) 
RET39=dailyReturn(Ad(SSE39)) 
RET40=dailyReturn(Ad(SSE40)) 
RET41=dailyReturn(Ad(SSE41)) 
RET42=dailyReturn(Ad(SSE42)) 
RET43=dailyReturn(Ad(SSE43)) 
RET44=dailyReturn(Ad(SSE44)) 
RET45=dailyReturn(Ad(SSE45)) 
RET46=dailyReturn(Ad(SSE46)) 
RET47=dailyReturn(Ad(SSE47)) 
RET48=dailyReturn(Ad(SSE48)) 
RET49=dailyReturn(Ad(SSE49)) 
RET50=dailyReturn(Ad(SSE50)) 

RET34 = read.csv(file.choose(),header = TRUE)
RET34 =xts(RET34$SSE33, as.Date(RET34$X, format='%Y/%m/%d'))




RET=merge(RETIN,RET01,RET02,RET03,RET04,RET05,RET06,RET07,RET08,RET09,RET10,
          RET11,RET12,RET13,RET14,RET15,RET16,RET17,RET18,RET19,RET20,
          RET21,RET22,RET23,RET24,RET25,RET26,RET27,RET28,RET29,RET30,
          RET31,RET32,RET33,RET34,RET35,RET36,RET37,RET38,RET39,RET40,
          RET41,RET42,RET43,RET44,RET45,RET46,RET47,RET48,RET49,RET50)

colnames(RET)=c("INDEX","SSE01","SSE02","SSE03","SSE04","SSE05","SSE06","SSE07","SSE08","SSE09","SSE10",
                "SSE11","SSE12","SSE13","SSE14","SSE15","SSE16","SSE17","SSE18","SSE19","SSE20",
                "SSE21","SSE22","SSE23","SSE24","SSE25","SSE26","SSE27","SSE28","SSE29","SSE30",
                "SSE31","SSE32","SSE33","SSE34","SSE35","SSE36","SSE37","SSE38","SSE39","SSE40",
                "SSE41","SSE42","SSE43","SSE44","SSE45","SSE46","SSE47","SSE48","SSE49","SSE50")

SSE50_RET=as.data.frame(RET)

write.csv(SSE50_RET, file = "E:上证50股票数据.csv", row.names = T, quote = F)


SSE50_RET =xts(SSE50_RET[,-1], as.Date(SSE50_RET[,1], format='%Y/%m/%d'))

