library(rpart)
library(rpart.plot)

data =read.csv(file.choose(),head=F)

reg<-rpart(V1~.,data)  
rpart.plot(reg, type=4, extra=1,
           shadow.col="gray", box.col="green",  
           border.col="blue", split.col="red",  
           split.cex=1.2, main="¾ö²ßÊ÷")
