library(spaceExt)
library(igraph)

data(finStocksCn)  ##data of finance sector of Chinese Stock Market, from January 4th, 2011 to July 26th, 2011
finStocksCn$names ##stock names

y.m<-scale(finStocksCn$returns)


CLOSE=read.csv(file.choose())
code=read.csv(file.choose())

sc<-scale(CLOSE[,-1])


# n=nrow(y.m)
# p=ncol(y.m)
res=glasso.miss(cc,rho=0.02,emIter=25,penalize.diagonal=FALSE)
# res$bic  ##bic returned

p=-res$wi
d=1/sqrt(diag(res$wi))
coc=-diag(d)%*%p%*%diag(d)
# par(mfrow=c(2,2))

colnames(coc)=re[,3]

g1=graph.adjacency(coc>0, mode="lower", weighted=NULL, diag=FALSE,
         add.colnames=NULL, add.rownames=NA)
 
   
# barplot(rep(1,times=10),col=c("#FFFF37","#00FF7F","#FF8000","#FF0080","#FF77FF","#4DFFFF","deepskyblue","chocolate","#FFAD86","#C07AB8"))
 
 cl=c("#FFFF37","#00FF7F","#FF8000","#FF0080","#FF77FF","#4DFFFF","deepskyblue","chocolate","#FFAD86","#C07AB8")
 com = walktrap.community(g1, steps = 3) #子群划分
 V(g1)$sg = com$membership + 1
# V(g1)$color = rainbow(max(V(g1)$sg))[V(g1)$sg]  #子群颜色
 V(g1)$color = cl[V(g1)$sg] 
  #V(g1)$size = 25 #子群大小
  #V(g1)[com$membership == 1]$size = 35
  #V(g1)[com$membership == 2]$size = 30
  #V(g1)[com$membership == 3]$size = 25
  #V(g1)[com$membership == 4]$size = 20
 
 
 
 #plot(g1,vertex.color = V(g1)$color)
 
  par(mar = c(0, 0, 0, 0))
  plot(g1,  layout = layout.fruchterman.reingold,
      #vertex.size=27,
      vertex.label.font=2,
      vertex.label.cex=1.0,
    # vertex.color=V(g1)$color,
      vertex.frame.color="white",
      vertex.label.color="black",
      edge.width=1,
      edge.label.font=1,
      edge.label.cex=0.9,
      edge.curved=TRUE,
      edge.color="#FFAD86",
      edge.arrow.size=0.2,
    # edge.label=re[,3],
      edge.label.color="#FFAD86",
      xlab="标题")



