library(jiebaRD)
library(jiebaR)
mixseg = worker()
##  相当于：
##       jiebar( type = "mix", dict = "inst/dict/jieba.dict.utf8",
##               hmm  = "inst/dict/hmm_model.utf8",  ### HMM模型数据
##               user = "inst/dict/user.dict.utf8") ### 用户自定义词库
keys = worker("keywords", topn = 50)
outkey=keys <= "C:\\Users\\shang\\Desktop\\一带一路-资讯\\微博.txt"
write.csv(outkey, file = "C:\\Users\\shang\\Desktop\\一带一路-资讯\\keys.csv", row.names = T, quote = T)
re=data.frame(outkey)
re[,2]=c("关键词")

re=read.csv(file.choose())
library(igraph)
g1 <- graph.data.frame(re, directed=TRUE, vertices=unique(c(as.vector(re[,1]),as.vector(re[,2]))))
cl <- c("#FFFF37","#00FF7F","#FF8000","#FF0080","#FF77FF","#4DFFFF","deepskyblue","chocolate","#FFAD86","#C07AB8")

# barplot(rep(1,times=n),col=c("#FFFF37","#00FF7F","#FF8000","#FF0080","#FF77FF","#4DFFFF","deepskyblue","chocolate","#FFAD86","#C07AB8"))

com = walktrap.community(g1, steps = 3) #子群划分
V(g1)$sg = com$membership + 1
V(g1)$color = rainbow(max(V(g1)$sg))[V(g1)$sg]  #子群颜色

V(g1)$size = 25 #子群大小
V(g1)[com$membership == 1]$size = 35
V(g1)[com$membership == 2]$size = 30
V(g1)[com$membership == 3]$size = 25
V(g1)[com$membership == 4]$size = 20



plot(g1,vertex.color = V(g1)$color)
par(mar = c(0, 0, 0, 0))
plot(g1,  layout = layout.fruchterman.reingold,
     #vertex.size=27,
     vertex.label.font=2,
     vertex.label.cex=1.0,
     vertex.color=V(g1)$color,
     vertex.frame.color="white",
     vertex.label.color="black",
     edge.width=2,
     edge.label.font=1,
     edge.label.cex=0.9,
     edge.curved=TRUE,
     edge.color=cl[9],
     edge.arrow.size=0.2,
   # edge.label=re[,3],
     edge.label.color= cl[33],
     xlab="标题")

library(arules)
g<-read.transactions("C:\\Users\\shang\\Desktop\\一带一路.csv",format="basket",sep=",")
inspect(g)
summary(g)
frequentsets=eclat(g,parameter=list(support=0.05,maxlen=10)) #求频繁项集
inspect(sort(frequentsets,by="support"))

rules=apriori(g,parameter=list(support=0.01,confidence=0.01)) #求关联规则
x=subset(rules,subset=rhs%in%"一带一路"&lift>=1.2)? ? #求所需要的关联规则子集
inspect(sort(x,by="confidence"))