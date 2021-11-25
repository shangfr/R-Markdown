
library(spaceExt)
library(igraph)

pm=read.csv(file.choose(),header = T)
rownames(pm)=pm$年份
pm=pm[,-1]
colnames(pm)=seq(1:17)

plot(pm)

#数据标准化
pm_sc<-scale(pm) 

cor_pm = cor(pm_sc)
g1=graph.adjacency(cor_pm>0.8, mode="lower", weighted=NULL, diag=FALSE,
                   add.colnames=NULL, add.rownames=NA)

cl=c("#FFFF37","#00FF7F","#FF8000","#FF0080","#FF77FF","#4DFFFF","deepskyblue","chocolate","#FFAD86","#C07AB8")
com = walktrap.community(g1, steps = 3) #子群划分
V(g1)$sg = com$membership + 1
V(g1)$color = cl[V(g1)$sg] 

par(mar = c(0, 0, 0, 0))
plot(g1,  layout = layout.fruchterman.reingold,
     vertex.size=16,
     vertex.label.font=1,
     vertex.label.cex=1,
     # vertex.color=V(g1)$color,
     vertex.frame.color="white",
     vertex.label.color="black",
     edge.width=1,
     edge.label.font=1,
     edge.label.cex=0.9,
     #  edge.curved=TRUE,
     edge.color="black",
     edge.arrow.size=0,
     # edge.label=re[,3],
     #  edge.label.color="#FFAD86",
     xlab="城市间PM2.5关联性-相关系数r>0.8")

leg.txt <- cleg.txt <- c(
  "1  商品房平均销售价格", 
  "2  常住人口(万人)", 
  "3  居民消费价格指数", 
  "4  生产总值(亿元)", 
  "5  商品房开发面积(万平方米)", 
  "6  商品房竣工面积(万平方米)", 
  "7  商品房销售面积(万平方米)", 
  "8  房地产开发投资额(亿元)", 
  "9  固定资产投资指数", 
  "10 居民消费水平(元)", 
  "11 人均家庭总收入(元)", 
  "12 人均可支配收入(元)", 
  "13 财政收入(亿元)", 
  "14 土地购置费（亿元）", 
  "15 城镇单位在岗职工平均工资", 
  "16 普通高等学校在校人数", 
  "17 建筑施工企业房屋建筑竣工面积(万平方米)")
legend(x=1.2,y=1.2,leg.txt, text.col = cl[V(g1)$sg],cex=0.8)

library(circlize)
M=as.matrix(pm)

chordDiagramFromMatrix(M,symmetric=FALSE)








pm_sc[6,2]=NA

res=glasso.miss(pm_sc,rho=0.05,emIter=10 ,penalize.diagonal=FALSE)
# res$bic  ##bic returned
p=-res$wi
d=1/sqrt(diag(res$wi))
coc=-diag(d)%*%p%*%diag(d)
colnames(coc)=colnames(pm_sc)
g=graph.adjacency(coc>0, mode="lower", weighted=NULL, diag=FALSE,
                  add.colnames=NULL, add.rownames=NA)

cl=c("#FFFF37","#00FF7F","#FF8000","#FF0080","#FF77FF","#4DFFFF","deepskyblue","chocolate","#FFAD86","#C07AB8")
com = walktrap.community(g, steps = 3) #子群划分
V(g)$sg = com$membership + 1
V(g)$color = cl[V(g)$sg] 


par(mar = c(0, 0, 0, 0))
plot(g,  layout = layout.fruchterman.reingold,
     vertex.size=6,
     vertex.label.font=1,
     vertex.label.cex=0.4,
     # vertex.color=V(g1)$color,
     vertex.frame.color="white",
     vertex.label.color="black",
     edge.width=1,
     edge.label.font=1,
     edge.label.cex=0.9,
     #  edge.curved=TRUE,
     edge.color="black",
     edge.arrow.size=0,
     # edge.label=re[,3],
     #  edge.label.color="#FFAD86",
     xlab="城市间PM2.5关联性-L1范数=0.5")
