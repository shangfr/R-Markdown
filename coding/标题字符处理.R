yy=read.csv(file.choose())
xx=data.frame()
n=dim(yy)[1]
for (i in 1:n) {
xx[i,1]=substring(yy[i,1], 
nchar(as.character(yy[i,1]))-15,nchar(as.character(yy[i,1]))-6)}

aa=table(xx)
plot(aa,col="red", lwd = 10,ylab="新闻数",xlab="时间",main="一带一路")
aa=as.data.frame(aa)
write.csv(aa, file = "E:yy.csv", row.names =F, quote = F)
