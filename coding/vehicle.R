base = read.csv("https://www.data.gouv.fr/s/resources/base-de-donnees-accidents-corporels-de-la-circulation-sur-6-annees/20150806-153355/vehicules_2014.csv")
T=table(base$Num_Acc)
idx=names(T)[which(T==2)]
nom=c("无","正面","右前","左前","追尾","右后","左后","右侧","左侧","多处")

 B=base[base$Num_Acc %in% idx,]  
 B=B[order(B$Num_Acc),]
 M=matrix(0,10,10)
 for(i in seq(1,nrow(B),by=2)){
   a=B$choc[i]+1
   b=B$choc[i+1]+1
   M[a,b]=M[a,b]+1
   M[b,a]=M[b,a]+1
 }
 rownames(M)=nom
 colnames(M)=nom

library(circlize)
chordDiagramFromMatrix(M,symmetric=TRUE)

rownames(M)=paste("A",nom,sep=" ")
colnames(M)=paste("B",nom,sep=" ")
chordDiagram(M)
coc=cor(M)
library(igraph)


g1=graph.adjacency(coc>0, mode="lower", weighted=NULL, diag=FALSE,
         add.colnames=NULL, add.rownames=NA)
# barplot(rep(1,times=10),col=c("#FFFF37","#00FF7F","#FF8000","#FF0080","#FF77FF","#4DFFFF","deepskyblue","chocolate","#FFAD86","#C07AB8"))
 
 cl=c("#FFFF37","#00FF7F","#FF8000","#FF0080","#FF77FF","#4DFFFF","deepskyblue","chocolate","#FFAD86","#C07AB8")
 com = walktrap.community(g1, steps = 2) #子群划分
 V(g1)$sg = com$membership + 1
V(g1)$color = rainbow(max(V(g1)$sg))[V(g1)$sg]  #子群颜色
#  V(g1)$color = cl[V(g1)$sg] 
  V(g1)$size = 25 #子群大小
  V(g1)[com$membership == 1]$size = 35
 V(g1)[com$membership == 2]$size = 30
  V(g1)[com$membership == 3]$size = 25
  V(g1)[com$membership == 4]$size = 20

plot(g1) 