require("gplots")
require("RColorBrewer")
sc=read.csv(file.choose(),head=T)
 pearson=cor(sc, method = "pearson") #("pearson", "kendall", "spearman")
 kendall=cor(sc, method = "kendall")
 spearman=cor(sc, method = "spearman")
a=as.vector(pearson)
b=as.vector(kendall)
c=as.vector(spearman)

# xlim=c(0,1),ylim=c(0,1500),c("#00FF7F","skyblue","tomato")
par(mfrow=c(2,2))
boxplot(sc[,-1],col=rainbow(52),main=NULL)


#hist(a,col="#00FF7F",xlab="Pearson-Cor",main=NULL)
#hist(b,col="skyblue",xlab="Kendall-Cor",main=NULL)
#hist(c,col="tomato",xlab="Spearman-Cor",main=NULL)
plot(density(a),col="#00FF7F",main="相关系数-概率密度函数",cex.main=1, lwd = 2)
lines(density(b),col="skyblue", lwd = 2)
lines(density(c),col="tomato", lwd = 2)


library(reshape2)
sc$Date=as.Date(sc$Date)
Data <- melt(sc, id="Date") # convert to long format
colnames(Data)=c("Date","Code","Price")

library(ggplot2)
#theme_gray() 查看主题参数
pp=ggplot(data=Data, aes(x=Date, y=Price, colour=Code))
pp+ geom_line(lty=1,size = 0.8)+labs(title = "Z-core标准化-股价走势图"
, xlab="Quality",ylab="Cumulative percetage of total reads"
)+theme(panel.background = element_rect(fill = "transparent",size=5, color = "gray")
,axis.text=element_text(face="bold",size=10),legend.key.size=unit(.2,'cm'),legend.text = element_text( size = 5))


mat_data=round(kendall,1)

my_palette <- colorRampPalette(c("#00FF7F","yellow","tomato" ))(n = 299)
col_breaks = c(seq(-1,0,length=100), seq(0,0.7,length=100), seq(0.7,1,length=100))

par(mar = c(40, 40, 1, 0.5))  # 设置边距参数和背景色
par(pin=c(20,30))  #定义图形为2英寸宽，3英寸高
par(lwd=2,cex=1.5)  #线条为默认的2倍宽，符号为默认的1.5倍
par(cex.axis=0.75,font.axis=3)  #坐标轴文字缩放为原来的75%，斜体

win.graph(width=800, height=800, pointsize=7)
heatmap.2(coc,# cellnote = mat_data same data set for cell labels
main = "Correlation", # heat map title 
notecol="black"  , #change font color of cell labels to black 
density.info="none", # turns off density plot inside color legend 
trace="none", # turns off trace lines inside the heat map
 margins =c(5,5), # widens margins around plot 
col=my_palette, # use on color palette defined earlier 
breaks=col_breaks, # enable color transition at specified limits 
dendrogram="none", 
Rowv=FALSE,
Colv=FALSE
) # turn off column clustering

win.graph(width=800, height=800, pointsize=7)
heatmap.2(mat_data,col=my_palette,trace="none")
savePlot("CTplot",type=c("jpg"),device=dev.cur(),restoreConsole=TRUE)
dev.off()

