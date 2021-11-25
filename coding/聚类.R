length(dtm)
dim(dtm) #看文本矩阵元素数
rownames(dtm) #看行向量名字
#Problem 1
#(a)
D=dim(dtm)[1]  #统计文本数
nw=dtm; nw[nw>0]=1; nw=colSums(nw)  #统计单词出现的文本数
mydtm1= dtm/rowSums(dtm)  #单词数标准化
dtm1=t(t(as.matrix(mydtm1))*log(D/nw)) #IDF权重

mydtm2=t(t(as.matrix(dtm))*log(D/nw)) #IDF权重
dtm2=mydtm2/rowSums(mydtm2)  #单词数标准化
#(b,c)
dtm3=dtm/rowSums(dtm)  
dist=sqrt(rowSums(scale(dtm3,center=T,scale=T)^2)) #矩阵中心标准化求和开方
q = c("but","cool","dude","party","michelangelo","raphael","rude")
mat = cbind(dtm3[,q],dist) #查看q向量内元素距离
colnames(mat) = c(q,"dist")
mat=data.frame(mat)
rownames(mat[mat$dist==min(mat$dist),])#显示与查询词组最接近的文本名

#根据距离聚类
out.dist=dist(dtm3,method="euclidean")
out.hclust=hclust(out.dist,method="complete") #根据距离聚类
plot(out.hclust)

#(d)
total=colSums(dtm) #单词汇总
t=sort(total,decreasing = TRUE) #排序
summary(t)


#Problem4
plot(x)

tree.sing = hclust(d,method="single")
tree.comp = hclust(d,method="complete")
tree.avg = hclust(d,method="average")

par(mfrow=c(2,3))
plot(tree.sing,labels=F,hang=-1e-10)
plot(tree.comp,labels=F,hang=-1e-10)
plot(tree.avg,labels=F,hang=-1e-10)

labs.sing = cutree(tree.sing,k=4)
labs.comp = cutree(tree.comp,k=4)
labs.avg = cutree(tree.avg,k=4)

cols = c("red","darkgreen","blue","green")
plot(x,col=cols[labs.sing])
plot(x,col=cols[labs.comp])
plot(x,col=cols[labs.avg])


