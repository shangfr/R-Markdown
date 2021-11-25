require(xts)
require(zoo)


require(quantmod)


stock=read.csv(file.choose(),header = TRUE)
stock[,1]=paste(stock$V1, "ss",sep=".")
codename=as.character(stock[,2])

setSymbolLookup(RZG=list(name=stock[1,1],src='yahoo'))	#	日照港	
setSymbolLookup(SGJT=list(name=stock[2,1],src='yahoo'))	#	上港集团	
setSymbolLookup(ZHFZ=list(name=stock[3,1],src='yahoo'))	#	中海发展	
setSymbolLookup(ZGSH=list(name=stock[4,1],src='yahoo'))	#	中国石化
setSymbolLookup(SYZG=list(name=stock[5,1],src='yahoo'))	#	三一重工	
setSymbolLookup(GZB=list(name=stock[6,1],src='yahoo'))	#	葛洲坝	
setSymbolLookup(TBDG=list(name=stock[7,1],src='yahoo'))	#	特变电工	
setSymbolLookup(TLWL=list(name=stock[8,1],src='yahoo'))	#	铁龙物流	
setSymbolLookup(ZGCB=list(name=stock[9,1],src='yahoo'))	#	中国船舶	
setSymbolLookup(JZG=list(name=stock[10,1],src='yahoo'))	#	锦州港	
setSymbolLookup(GHNY=list(name=stock[11,1],src='yahoo'))	#	广汇能源	
setSymbolLookup(CQGJ=list(name=stock[12,1],src='yahoo'))	#	重庆港九	
setSymbolLookup(PGDQ=list(name=stock[13,1],src='yahoo'))	#	平高电气	
setSymbolLookup(YKG=list(name=stock[14,1],src='yahoo'))	#	营口港	
setSymbolLookup(ZHZG=list(name=stock[15,1],src='yahoo'))	#	振华重工	
setSymbolLookup(WZJT=list(name=stock[16,1],src='yahoo'))	#	五洲交通	
setSymbolLookup(GDNR=list(name=stock[17,1],src='yahoo'))	#	国电南瑞	
setSymbolLookup(QSJH=list(name=stock[18,1],src='yahoo'))	#	青松建化	
setSymbolLookup(ZYHY=list(name=stock[19,1],src='yahoo'))	#	中远航运	
setSymbolLookup(JXCZ=list(name=stock[20,1],src='yahoo'))	#	晋西车轴	
setSymbolLookup(ZTKJ=list(name=stock[21,1],src='yahoo'))	#	中天科技	
setSymbolLookup(ZTEJ=list(name=stock[22,1],src='yahoo'))	#	中铁二局	
setSymbolLookup(XJCJ=list(name=stock[23,1],src='yahoo'))	#	新疆城建	
setSymbolLookup(WLDQ=list(name=stock[24,1],src='yahoo'))	#	卧龙电气	
setSymbolLookup(HYGC=list(name=stock[25,1],src='yahoo'))	#	海油工程	
setSymbolLookup(TJG=list(name=stock[26,1],src='yahoo'))	#	天津港	
setSymbolLookup(NBHY=list(name=stock[27,1],src='yahoo'))	#	宁波海运	
setSymbolLookup(SDGF=list(name=stock[28,1],src='yahoo'))	#	隧道股份	
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
setSymbolLookup(ZGZG=list(name=stock[50,1],src='yahoo'))	#	中国重工	
setSymbolLookup(KNJD=list(name=stock[51,1],src='yahoo'))	#	康尼机电	
setSymbolLookup(YLGF=list(name=stock[52,1],src='yahoo'))	#	应流股份	

getSymbols(codename,from = "2015-01-01",to = Sys.Date(),src = "yahoo",auto.assign=TRUE)
YDYL=cbind(RZG, SGJT, ZHFZ, ZGSH, SYZG, GZB,  TBDG, TLWL, ZGCB, JZG,  GHNY, CQGJ, PGDQ, YKG,
           ZHZG, WZJT, GDNR, QSJH, ZYHY, JXCZ, ZTKJ, ZTEJ, XJCJ, WLDQ, HYGC, TJG,  NBHY, SDGF,
           DFDQ, ZCGJ, JYSY, LYG,  NBG,  ZGYZ, ZGHX, ZGXD, ZGTJ, ZGZT, ZGZZ, ZGJZ, ZGDJ, SHDQ,
           ZGNC, ZGJJ, ZHYF, ZGSY, ZHJY, ZSLC, DLG,  ZGZG, KNJD, YLGF)

YDYL=as.data.frame(YDYL)

CLOSE=YDYL[,seq(4,312,6)]
CLOSE=as.data.frame(CLOSE)

write.csv(YDYL, file = "E:一带一路.csv", row.names = T, quote = F)
write.csv(CLOSE, file = "E:收盘价.csv", row.names = T, quote = F)


getSymbols(stock[2,1],from = "2015-04-01",to = Sys.Date(),src = "yahoo",auto.assign=FALSE, env = new.environment)